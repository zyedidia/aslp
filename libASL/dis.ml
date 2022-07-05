(****************************************************************
 * ASL dissassembler
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL dissassembler *)

module PP   = Asl_parser_pp
module AST  = Asl_ast
module TC   = Tcheck

open AST
open Asl_utils
open Value
open Eval

type result_or_simplified =
    | Result of value
    | Simplified of AST.expr

type 'a writer = 'a * stmt list * Env.t

let return x = (x, [], Env.empty)

let write s = ((),[s], Env.empty)

let write_multi ss = ((), ss, Env.empty)

let ( let* ) (x: 'a writer) (f: 'a -> 'b writer): 'b writer =
    let (ex, stmts1, env1) = x in
    let (ex', stmts2, env2) = f ex in
    (ex', stmts1 @ stmts2, env1)

let (let**) (xs: ('a writer) list) (f: 'a list -> 'b writer): 'b writer =
    let xsa = List.map (fun (a, _, _) -> a) xs in
    let xsbs = List.concat (List.map (fun (_, bs, _) -> bs) xs) in
    let (ex', stmts, env) = f xsa in
    (ex', xsbs @ stmts, env)

let read (w: 'a writer): stmt list =
    match w with (_, stmts, _) -> stmts

let pp_result_or_simplified (rs: result_or_simplified): string = 
    match rs with
    | Result v -> pp_value v
    | Simplified e -> pp_expr e

(** Convert value to a simple expression containing that value, so we can
    print it or use it symbolically *)
let rec to_expr (v: result_or_simplified): AST.expr = 
    match v with
    | Result x ->
        (match x with 
        | VBool b -> Expr_LitInt(if b then "1" else "0")
        | VEnum (id, n) -> Expr_LitInt(string_of_int n)
        | VInt n -> Expr_LitInt(string_of_int (Z.to_int n))
        | VReal n -> Expr_LitReal(string_of_float (Q.to_float n))
        | VBits {n; v} -> Expr_LitInt(string_of_int (Z.to_int v))
        | VString s -> Expr_LitString(s)
        | VTuple vs -> Expr_Tuple(List.map (fun v -> to_expr (Result v)) vs)
        (* TODO: definitely get rid of this. Should convert every case *)
        | x -> Printf.printf "WARNING: converting unknown structure to 0\n"; Expr_LitInt("0")
        )
    | Simplified x -> x

let is_val (x: AST.expr): bool =
    (match x with 
    | Expr_LitInt _ -> true
    | Expr_LitReal _ -> true
    | Expr_LitString _ -> true
    | Expr_Tuple _ -> true
    | x -> false
    )

(** Converts a result_or_simplified to a value. 
    Raises an exception if an expression is given, as an expression cannot be casted to a value. 
    Requires checking beforehand *)
let to_value (v: result_or_simplified): value = 
    match v with 
    | Result v' -> v' 
    | Simplified _ -> raise (EvalError (Unknown, "Unreachable"))

let is_expr (v: result_or_simplified): bool =
    match v with
    | Result _ -> false
    | Simplified _ -> true

let contains_expr (xs: result_or_simplified list): bool =
    List.exists is_expr xs

(** Dissassemble type *)
let rec dis_type (loc: l) (env: Env.t) (t: ty): ty writer =
    match t with
    | Type_Bits ex ->
        let* ex' = dis_expr loc env ex in
        return (Type_Bits (match ex' with | Result v -> to_expr (Result v) | Simplified x -> x))
    | Type_OfExpr ex ->
        let* ex' = dis_expr loc env ex in
        return (Type_OfExpr (match ex' with | Result v -> to_expr (Result v) | Simplified x -> x))
    | Type_Tuple tys ->
        let** exprs = List.map (dis_type loc env) tys in
        return (Type_Tuple (exprs))
    | t' -> return t'

(** Dissassemble list of expressions *)
and dis_exprs (loc: l) (env: Env.t) (xs: AST.expr list): result_or_simplified list writer =
    let** exprs = List.map (dis_expr loc env) xs in return exprs

(** Evaluate bitslice bounds *)
and dis_slice (loc: l) (env: Env.t) (x: AST.slice): (result_or_simplified * result_or_simplified) writer =
    (match x with
    | Slice_Single(i) ->
            let* i' = dis_expr loc env i in
            return (i', Result (VInt Z.one))
    | Slice_HiLo(hi, lo) ->
            let* hi' = dis_expr loc env hi in
            let* lo' = dis_expr loc env lo in
            (match hi' with
            | Result vh -> 
                (match lo' with
                | Result vl -> return (Result vl, Result (eval_add_int loc (eval_sub_int loc vh vl) (VInt Z.one)))
                | Simplified el -> return (Simplified el, Simplified (Expr_Binop(Expr_Binop(to_expr (Result vh), Binop_Minus, el), Binop_Plus, to_expr (Result (VInt Z.one)))))
                )
            | Simplified eh -> 
                (match lo' with
                | Result vl -> return (Result vl, Simplified (Expr_Binop(Expr_Binop(eh, Binop_Minus, to_expr (Result vl)), Binop_Plus, to_expr (Result (VInt Z.one)))))
                | Simplified el -> return (Simplified el, Simplified (Expr_Binop(Expr_Binop(eh, Binop_Minus, el), Binop_Plus, to_expr (Result (VInt Z.one)))))
                )
            )
    | Slice_LoWd(lo, wd) ->
            let* lo' = dis_expr loc env lo in
            let* wd' = dis_expr loc env wd in
            return (lo', wd')
    )

(** Dissassemble a function call *)
and dis_fun (loc: l) (env: Env.t) (f: ident) (tes: AST.expr list) (es: AST.expr list): result_or_simplified writer =
    if name_of_FIdent f = "and_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> 
            let* x' = dis_expr loc env x in
            (match x' with
            | Result v -> 
                if to_bool loc v then dis_expr loc env y else return (Result (from_bool false))
            | Simplified e -> 
                let* y' = dis_expr loc env y in
                return (Simplified (Expr_TApply(f, tes, [e; to_expr y'])))
            )
        | _ ->
            raise (EvalError (loc, "malformed and_bool expression"))
        )
    end else if name_of_FIdent f = "or_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> 
            let* x' = dis_expr loc env x in
            (match x' with
            | Result v -> 
                if to_bool loc v then return (Result (from_bool true)) else dis_expr loc env y
            | Simplified e -> 
                let* y' = dis_expr loc env y in
                return (Simplified (Expr_TApply(f, tes, [e; to_expr y'])))
            )
        | _ ->
            raise (EvalError (loc, "malformed or_bool expression"))
        )
    end else if name_of_FIdent f = "implies_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> 
            let* x' = dis_expr loc env x in
            (match x' with
            | Result v -> 
                if to_bool loc v then dis_expr loc env y else return (Result (from_bool true))
            | Simplified e -> 
                let* y' = dis_expr loc env y in
                return (Simplified (Expr_TApply(f, tes, [e; to_expr y'])))
            )
        | _ ->
            raise (EvalError (loc, "malformed implies_bool expression"))
        )
    end else (match (try (Some (Env.getFun loc env f)) with EvalError _ -> None) with
    | Some (rty, atys, targs, args, loc, b) ->

        (* Add return type variables *)
        (* For each of these, if one already exists, add it to a stack to be restored after *)
        Env.addImplicitLevel env;
        let* _ =
        write_multi (List.concat (List.map2 (fun arg e -> 
            read (
                let* e' = dis_expr loc env e in
                (try (Env.addImplicitValue env arg (Env.getVar loc env arg)) with EvalError _ -> ());
                (match e' with
                | Result v -> return (Env.addLocalVar loc env arg v)
                | Simplified _ -> return ()
                )
            ) 
        ) targs tes)) in

        (* Add return variable declarations *)
        let fName = name_of_FIdent f in
        let varNames = List.map (fun n -> Ident (fName ^ "Var" ^ string_of_int n ^ string_of_int (Env.getNumSymbols env))) (Utils.range 0 (List.length targs)) in
        let* _ = (match rty with 
            | Some t -> 
                let* t' = dis_type loc env t in
                write (Stmt_VarDeclsNoInit(
                    t',
                    varNames, 
                    Unknown
                ))
            | None -> 
                write (Stmt_VarDeclsNoInit(
                    Type_Constructor (Ident "ERRORType"),
                    varNames, 
                    Unknown
                ))
        ) in

        (* Create the return symbol *)
        let rv = (match varNames with
            | [] -> Expr_Tuple []
            | [name] -> Expr_Var(name)
            | names -> Expr_Tuple(List.map (fun n -> Expr_Var n) names)) in

        (* Add local parameters, avoiding name collisions *)
        (* Also print what the parameter refers to *)
        let localPrefix = fName ^ string_of_int (Env.getNumSymbols env) in
        let* _ = write_multi (List.concat (Utils.map3 (fun (ty, _) arg ex -> 
            read (
                let* ex' = dis_expr loc env ex in 
                (match ex' with 
                | Result v -> Env.addLocalVar loc env (Ident (localPrefix ^ pprint_ident arg)) v
                | Simplified ex'' -> Env.addLocalVar loc env (Ident (localPrefix ^ pprint_ident arg)) VUninitialized
                );
                let* ty' = dis_type loc env ty in
                write (Stmt_VarDecl(ty', Ident (localPrefix ^ (pprint_ident arg)), to_expr ex', loc))
            )
        ) atys args es)) in

        (* print out the body *)
        Env.addReturnSymbol env rv;
        Env.addLocalPrefix env localPrefix;
        let* _ = dis_stmts env b in
        Env.removeReturnSymbol env;
        Env.removeLocalPrefix env;

        (* Restore implicit values *)
        List.iter (fun (arg, v) -> Env.addLocalVar loc env arg v) (Env.getImplicitLevel env);

        (* Return the return symbol to be used in expressions e.g. assignments *)
        return (Simplified rv)
    | None ->
        let* tes' = dis_exprs loc env tes in
        let* es' = dis_exprs loc env es in
        (match eval_prim 
            (name_of_FIdent f) 
            (List.map (fun v -> match v with | Result v -> v | Simplified _ -> VUninitialized) tes') 
            (List.map (fun v -> match v with | Result v -> v | Simplified _ -> VUninitialized) es') with
        | Some VUninitialized -> return (Simplified (Expr_TApply(f, List.map to_expr tes', List.map to_expr es')))
        | Some v -> return (Result v)
        | None -> return (Simplified (Expr_TApply(f, List.map to_expr tes', List.map to_expr es')))
        )
    )

(** Dissassemble expression. This should never return Result VUninitialized *)
and dis_expr (loc: l) (env: Env.t) (x: AST.expr): result_or_simplified writer =
    match x with
    | Expr_If(c, t, els, e) ->
        let rec eval_if xs d = match xs with
            | [] -> dis_expr loc env d
            (* If we cannot evaluate the condition, print the whole statement *)
            | AST.E_Elsif_Cond (cond, b)::xs' -> 
                let* cond' = dis_expr loc env cond in
                match cond' with
                | Result v ->
                    if to_bool loc v then
                        (* Just print this branch *)
                        dis_expr loc env b
                    else
                        (* Print whatever the rest of the branches turn out to be *)
                        eval_if xs' d
                | Simplified cond'' -> 
                    let* b' = dis_expr loc env b in
                    let* els' = dis_if_expr_no_remove loc env els in
                    let* e' = dis_expr loc env e in
                    return (Simplified (Expr_If(
                        cond'', 
                        to_expr (b'),
                        els',
                        to_expr (e')
                    )))
        in
        eval_if (E_Elsif_Cond(c, t)::els) e
    (* NOTE: This does not consider early returns currently. It also doesn't handle recursive calls *)
    | Expr_TApply(f, tes, es) ->
        dis_fun loc env f tes es
    | Expr_Var id ->
        (try 
            (match (Env.getVar loc env (Ident ((Env.getLocalPrefix loc env) ^ pprint_ident id))) with 
            | VUninitialized -> return (Simplified (Expr_Var(Ident ((Env.getLocalPrefix loc env) ^ pprint_ident id))))
            | v -> return (Result v))
        with EvalError _ ->
            (try 
                (match (Env.getVar loc env id) with 
                | VUninitialized -> return (Simplified x)
                | v -> return (Result v))
            with EvalError _ -> return (Simplified x)))
    | Expr_In(e, p) ->
        let* e' = dis_expr loc env e in
        (match e' with
        | Result v -> return (Result (from_bool (eval_pattern loc env v p)))
        | Simplified _ -> return (Simplified x))
    | Expr_Slices(e, ss) ->
        let** transformedSlices = List.map (fun s -> dis_slice loc env s) ss in
        let* e' = dis_expr loc env e in
        (match e' with
        | Result v ->
            if List.exists (fun ts -> match ts with (Simplified _, _) -> true | (_, Simplified _) -> true | (_, _) -> false) transformedSlices then
                return (Simplified (Expr_Slices(to_expr (Result v), List.map (fun (i, w) -> Slice_HiLo(to_expr i, to_expr w)) transformedSlices)))
            else
                let vs = List.map (fun s -> 
                    (match s with
                    | (Result v1, Result v2) -> extract_bits loc v v1 v2
                    | _ -> raise (EvalError (loc, "Unreachable: Shouldn't have expression in bit slice\n")))
                    ) transformedSlices in
                    return (Result (eval_concat loc vs))
        | Simplified e' -> 
            return (Simplified (Expr_Slices(e', List.map2 (fun (i, w) s ->
                (match s with
                | Slice_Single _ -> Slice_Single(to_expr i)
                | Slice_HiLo _ -> Slice_HiLo(to_expr i, to_expr w)
                | Slice_LoWd _ -> Slice_LoWd(to_expr i, to_expr w)
                )
            ) transformedSlices ss))))
    | Expr_Tuple(es) ->
        let** transformedExprs = List.map (dis_expr loc env) es in
        if contains_expr transformedExprs then
            return (Simplified (Expr_Tuple(List.map to_expr transformedExprs)))
        else
            return (Result (VTuple (List.map to_value transformedExprs)))
    | Expr_Array(a, i) ->
        let* a' = dis_expr loc env a in
        let* i' = dis_expr loc env i in
        if is_expr a' || is_expr i' then
            return (Simplified (Expr_Array(a, to_expr i')))
        else
            (match (get_array loc (to_value a') (to_value i')) with
            | VUninitialized -> return (Simplified (Expr_Array(a, to_expr i')))
            | v -> return (Result v))
    | x -> try (match eval_expr loc env x with VUninitialized -> return (Simplified x) | v -> return (Result v)) with EvalError (loc, message) -> return (Simplified x)

(** Evaluate and simplify guards and bodies of an elseif chain, without removing branches *)
and dis_if_expr_no_remove (loc: l) (env: Env.t) (xs: e_elsif list): e_elsif list writer = 
    match xs with
    | [] -> return []
    | (AST.E_Elsif_Cond (cond, b)::xs') ->
        let* cond' = dis_expr loc env cond in
        let* b' = dis_expr loc env b in
        let* els' = dis_if_expr_no_remove loc env xs' in
        return (AST.E_Elsif_Cond(
            to_expr (cond'), 
            to_expr (b')
        ) :: (els'))

and dis_lexpr (loc: l) (env: Env.t) (x: AST.lexpr) (r: result_or_simplified): unit writer =
    match x with
    | LExpr_Write(setter, tes, es) ->
        let** tvs = List.map (dis_expr loc env) tes in
        let** vs = List.map (dis_expr loc env) es in
        if contains_expr tvs || contains_expr vs || contains_expr [r] then
            write (Stmt_Assign(LExpr_Write(setter, List.map to_expr tvs, List.map to_expr vs), to_expr r, loc))
        else begin
            return (eval_proccall 
            loc 
            env 
            setter 
            (List.map (to_value) tvs) 
            ((List.map to_value vs) @ [to_value r]))
        end
    | LExpr_Var(v) ->
        let v' = try 
            (ignore (Env.getVar loc env (Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v)))); 
            (Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v)))) 
        with EvalError _ -> v in
        (match r with
        | Result r' -> 
            return (Env.setVar loc env v' r')
        | Simplified e -> 
            Env.setVar loc env v' VUninitialized;
            write (Stmt_Assign(LExpr_Var(v'), e, loc)))
    | _ -> try (return (eval_lexpr loc env x (to_value r))) with EvalError _ ->
        write (Stmt_Assign(x, to_expr r, loc))

(** Dissassemble list of statements *)
and dis_stmts (env: Env.t) (xs: AST.stmt list): unit writer =
    write_multi (List.concat (List.map read (List.map (dis_stmt env) xs)))

(** Evaluate and simplify guards and bodies of an elseif chain, without removing branches *)
and dis_if_stmt_no_remove (loc: l) (env: Env.t) (xs: s_elsif list): s_elsif list writer = 
    match xs with
    | [] -> return []
    | (AST.S_Elsif_Cond (cond, b)::xs') ->
        let* cond' = dis_expr loc env cond in
        let* els' = dis_if_stmt_no_remove loc env xs' in
        let b' = dis_stmts env b in
        return (AST.S_Elsif_Cond(to_expr cond', read (b'))::(els'))

(** Disassemble statement *)
and dis_stmt (env: Env.t) (x: AST.stmt): unit writer =
    (match x with
    | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        (* If a local prefix exists, add it *)
        let vs' = try (List.map (fun v -> Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v))) vs) with EvalError _ -> vs in
        (* Add the variables *)
        List.iter (fun v -> Env.addLocalVar loc env v VUninitialized) vs';
        (* Print the declarations *)
        let* ty' = dis_type loc env ty in
        write (Stmt_VarDeclsNoInit(ty', vs', loc))
    | Stmt_VarDecl(ty, v, i, loc) ->
        (* If a local prefix exists, add it *)
        let v' = try Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v)) with EvalError _ -> v in
        (* Add the variable *)
        let* i' = dis_expr loc env i in
        (match i' with
        | Result i' -> return (Env.addLocalVar loc env v' i')
        | Simplified ex ->
            Env.addLocalVar loc env v' VUninitialized;
            let* ty' = dis_type loc env ty in
            write (Stmt_VarDecl(ty', v', ex, loc))
        )
    | Stmt_ConstDecl(ty, v, i, loc) ->
        (* If a local prefix exists, add it *)
        let v' = try Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v)) with EvalError _ -> v in
        let* i' = dis_expr loc env i in
        (match i' with
        | Result i' -> return (Env.addLocalConst loc env v' i')
        | Simplified ex -> 
            (* Declare constant with uninitialized value and just use symbolically *)
            Env.addLocalConst loc env v' VUninitialized;
            let* ty' = dis_type loc env ty in
            write (Stmt_ConstDecl(ty', v', ex, loc))
        )
    | Stmt_Assign(l, r, loc) ->
        let* r' = dis_expr loc env r in
        dis_lexpr loc env l r' (* TODO: double check that both statements are added *)
    | Stmt_If(c, t, els, e, loc) ->
        let rec eval_if xs d = match xs with
            | [] -> dis_stmts env e
            | AST.S_Elsif_Cond (cond, b)::xs' ->
                let* cond' = dis_expr loc env cond in
                (match cond' with
                | Result v -> 
                    if to_bool loc v then
                        (* Just print this branch *)
                        dis_stmts env b
                    else
                        (* Print whatever the rest of the branches turn out to be *)
                        eval_if xs' d
                (* We have to print out all branches now because we don't know
                   whether or not this one is true. We can still simplify 
                   guards and bodies though *)
                | Simplified cond'' -> 
                    let t' = read (dis_stmts env t) in
                    let* els' = dis_if_stmt_no_remove loc env els in
                    let e' = read (dis_stmts env e) in
                    write (Stmt_If(cond'', t', els', e', loc))
                )
        in
        eval_if (S_Elsif_Cond(c, t)::els) e
    | Stmt_FunReturn(e, loc) ->
        let* e' = dis_expr loc env e in
        write (Stmt_Assign((match (Env.getReturnSymbol loc env) with Expr_Var(i) -> LExpr_Var(i) | _ -> raise (EvalError (loc, "TODO"))), to_expr e', loc))
    | Stmt_Assert(e, loc) ->
        let* e' = dis_expr loc env e in
        (match e' with 
        | Result v -> return (if not (to_bool loc v) then
            raise (EvalError (loc, "assertion failure")))
        | Simplified e'' ->
            write (Stmt_Assert(e'', loc))
        )
    | Stmt_Case(e, alts, odefault, loc) ->
        (let rec eval v alts =
            (match alts with
            | [] ->
                    (match odefault with
                    | None -> raise (EvalError (loc, "unmatched case"))
                    | Some s -> dis_stmts env s
                    )
            | (Alt_Alt(ps, oc, s) :: alts') ->
                    if List.exists (eval_pattern loc env v) ps && Utils.from_option
                    (Utils.map_option (to_bool loc) (Utils.map_option (eval_expr loc env) oc)) (fun _ -> true) then
                        dis_stmts env s
                    else
                        eval v alts'
            )
        in
        let* e' = dis_expr loc env e in
        (match e' with
        | Result v -> eval v alts
        | Simplified _ ->
            (* TODO simplify cases individually*)
            write x
        ))
    | x -> write x
    )

(* Duplicate of eval_decode_case modified to print rather than eval *)
let rec dis_decode_case (loc: AST.l) (env: Env.t) (x: decode_case) (op: value): unit =
    (match x with
    | DecoderCase_Case (ss, alts, loc) ->
            let vs = List.map (fun s -> eval_decode_slice loc env s op) ss in
            let rec dis alts =
                (match alts with
                | (alt :: alts') ->
                    if dis_decode_alt loc env alt vs op then () else dis alts'
                | [] ->
                        raise (EvalError (loc, "unmatched decode pattern"))
                )
            in
            dis alts
    )

(* Duplicate of eval_decode_alt modified to print rather than eval *)
and dis_decode_alt (loc: AST.l) (env: Env.t) (DecoderAlt_Alt (ps, b)) (vs: value list) (op: value): bool =
    if List.for_all2 (eval_decode_pattern loc) ps vs then
        (match b with
        | DecoderBody_UNPRED loc -> raise (Throw (loc, Exc_Unpredictable))
        | DecoderBody_UNALLOC loc -> raise (Throw (loc, Exc_Undefined))
        | DecoderBody_NOP loc -> raise (Throw (loc, Exc_Undefined))
        | DecoderBody_Encoding (inst, l) -> 
                let (enc, opost, cond, exec) = Env.getInstruction loc env inst in
                if eval_encoding env enc op then begin
                    (match opost with
                    | Some post -> List.iter (function s -> Printf.printf "%s\n" (pp_stmt s)) post;
                        (*List.iter (eval_stmt env) post*)
                    | None -> ()
                    );
                    (* todo: should evaluate ConditionHolds to decide whether to execute body *)
                    Printf.printf "Dissasm: %s\n" (pprint_ident inst);
                    (* Uncomment this if you want to see output with no evaluation *)
                    (* List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) exec; *)
                    let stmts = read (dis_stmts env exec) in
                    (* List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) stmts; *)
                    List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) (join_decls (remove_unused (constant_propagation stmts)));
                    true
                end else begin
                    false
                end
        | DecoderBody_Decoder (fs, c, loc) ->
                let env = Env.empty in (* todo: this seems to share a single mutable object far too widely *)
                List.iter (function (IField_Field (f, lo, wd)) ->
                    Env.addLocalVar loc env f (extract_bits' loc op lo wd)
                ) fs;
                dis_decode_case loc env c op;
                true
        )
    else
      false

and remove_unused (xs: stmt list): stmt list =
    match List.fold_right (fun stmt (acc, idents) -> 
        let stmts =
            match stmt with
            | Stmt_VarDeclsNoInit(ty, vs, loc) -> 
                (match List.filter (fun ident -> IdentSet.mem ident idents) vs with
                | [] -> acc
                | xs -> Stmt_VarDeclsNoInit(ty, xs, loc) :: acc
                )
            | Stmt_VarDecl(ty, v, i, loc) -> if IdentSet.mem v idents then stmt :: acc else acc
            | Stmt_ConstDecl(ty, v, i, loc) -> if IdentSet.mem v idents then stmt :: acc else acc
            | Stmt_Assign(LExpr_Var(v), r, loc) -> if IdentSet.mem v idents then stmt :: acc else acc
            | x -> x :: acc
        in
        let newIdents = IdentSet.union idents (fv_stmt stmt) in
        (stmts, newIdents)
    ) xs ([], IdentSet.empty) with (acc, idents) -> acc

and constant_propagation (xs: stmt list): stmt list =
    match List.fold_left (fun (acc, bs) stmt -> 
        match stmt with
        | Stmt_VarDecl(ty, v, i, loc) -> if is_val i then (acc, Bindings.add v i bs) else (acc @ [subst_stmt bs stmt], bs)
        | Stmt_ConstDecl(ty, v, i, loc) -> if is_val i then (acc, Bindings.add v i bs) else (acc @ [subst_stmt bs stmt], bs)
        | Stmt_Assign(LExpr_Var(v), r, loc) -> if is_val r then (acc, Bindings.add v r bs) else (acc @ [subst_stmt bs stmt], bs)
        | x -> (acc @ [subst_stmt bs stmt], bs)
    ) ([], Bindings.empty) xs with (acc, bs) -> acc

(* Don't print no init decls until they are assigned *)
and join_decls (xs: stmt list): stmt list =
    match List.fold_left (fun (acc, bs) stmt -> 
        (match stmt with
        | Stmt_VarDeclsNoInit(ty, vs, loc) ->
            (acc, List.fold_left (fun bs v -> Bindings.add v ty bs) bs vs)
        | Stmt_Assign(LExpr_Var(ident), r, loc) -> 
            if Bindings.mem ident bs then 
                (acc @ [Stmt_VarDecl(Bindings.find ident bs, ident, r, loc)], Bindings.remove ident bs)
            else
                (acc @ [stmt], bs)
        | _ -> (acc @ [stmt], bs)
        )
    ) ([], Bindings.empty) xs with (acc, bs) -> acc
