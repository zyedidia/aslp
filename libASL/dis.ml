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

(** Concatenate statements inside a result_or_simplified tuple *)
let concat_stmts (stmts1: stmt list) ((ros, stmts2): (result_or_simplified * stmt list)): (result_or_simplified * stmt list) =
    (ros, stmts1 @ stmts2)

(** Dissassemble type *)
let rec dis_type (loc: l) (env: Env.t) (t: ty): ty * stmt list =
    match t with
    | Type_Bits ex ->
        let (ex', stmts) = dis_expr loc env ex in
        (Type_Bits (match ex' with | Result v -> to_expr (Result v) | Simplified x -> x), stmts)
    | Type_OfExpr ex ->
        let (ex', stmts) = dis_expr loc env ex in
        (Type_OfExpr (match ex' with | Result v -> to_expr (Result v) | Simplified x -> x), stmts)
    | Type_Tuple tys ->
        let exprsstmts = List.map (dis_type loc env) tys in
        let exprs = List.map (fun (e, _) -> e) exprsstmts in
        let stmts = List.map (fun (_, s) -> s) exprsstmts in
        (Type_Tuple (exprs), List.concat stmts)
    | t' -> (t', [])

(** Dissassemble list of expressions *)
and dis_exprs (loc: l) (env: Env.t) (xs: AST.expr list): result_or_simplified list * stmt list =
    let exprsstmts = List.map (dis_expr loc env) xs in
    let exprs = List.map (fun (e, _) -> e) exprsstmts in
    let stmts = List.map (fun (_, s) -> s) exprsstmts in
    (exprs, List.concat stmts)

(** Evaluate bitslice bounds *)
and dis_slice (loc: l) (env: Env.t) (x: AST.slice): (result_or_simplified * result_or_simplified) * stmt list =
    (match x with
    | Slice_Single(i) ->
            let (i', stmts) = dis_expr loc env i in
            ((i', Result (VInt Z.one)), stmts)
    | Slice_HiLo(hi, lo) ->
            let (hi', histmts) = dis_expr loc env hi in
            let (lo', lostmts) = dis_expr loc env lo in
            (match hi' with
            | Result vh -> (match lo' with
                | Result vl -> ((Result vl, Result (eval_add_int loc (eval_sub_int loc vh vl) (VInt Z.one))), histmts @ lostmts)
                | Simplified el -> ((Simplified el, Simplified (Expr_Binop(Expr_Binop(to_expr (Result vh), Binop_Minus, el), Binop_Plus, to_expr (Result (VInt Z.one)))))), histmts @ lostmts)
            | Simplified eh -> (match lo' with
                | Result vl -> ((Result vl, Simplified (Expr_Binop(Expr_Binop(eh, Binop_Minus, to_expr (Result vl)), Binop_Plus, to_expr (Result (VInt Z.one))))), histmts @ lostmts)
                | Simplified el -> ((Simplified el, Simplified (Expr_Binop(Expr_Binop(eh, Binop_Minus, el), Binop_Plus, to_expr (Result (VInt Z.one)))))), histmts @ lostmts))
    | Slice_LoWd(lo, wd) ->
            let (lo', lostmts) = dis_expr loc env lo in
            let (wd', wdstmts) = dis_expr loc env wd in
            ((lo', wd'), lostmts @ wdstmts)
    )

(** Dissassemble a function call *)
and dis_fun (loc: l) (env: Env.t) (f: ident) (tes: AST.expr list) (es: AST.expr list): result_or_simplified * stmt list =
    if name_of_FIdent f = "and_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> (match dis_expr loc env x with
            | (Result v, stmts) -> if to_bool loc v then concat_stmts stmts (dis_expr loc env y) else (Result (from_bool false), stmts)
            | (Simplified e, stmts) -> 
                let (y', ystmts) = dis_expr loc env y in
                (Simplified (Expr_TApply(f, tes, [e; to_expr y']))), stmts @ ystmts)
        | _ ->
            raise (EvalError (loc, "malformed and_bool expression"))
        )
    end else if name_of_FIdent f = "or_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> 
            (match dis_expr loc env x with
            | (Result v, stmts) -> if to_bool loc v then (Result (from_bool true), stmts) else concat_stmts stmts (dis_expr loc env y)
            | (Simplified e, stmts) -> 
                let (y', ystmts) = dis_expr loc env y in
                (Simplified (Expr_TApply(f, tes, [e; to_expr y']))), stmts @ ystmts)
        | _ ->
            raise (EvalError (loc, "malformed or_bool expression"))
        )
    end else if name_of_FIdent f = "implies_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> (match dis_expr loc env x with
            | (Result v, stmts) -> if to_bool loc v then concat_stmts stmts (dis_expr loc env y) else (Result (from_bool true), stmts)
            | (Simplified e, stmts) -> 
                let (y', ystmts) = dis_expr loc env y in
                (Simplified (Expr_TApply(f, tes, [e; to_expr y']))), stmts @ ystmts)
        | _ ->
            raise (EvalError (loc, "malformed implies_bool expression"))
        )
    end else (match (try (Some (Env.getFun loc env f)) with EvalError _ -> None) with
        | Some (rty, atys, targs, args, loc, b) ->

            (* Setup names for return symbols*)
            let fName = name_of_FIdent f in
            let varNames = List.map (fun n -> Ident (fName ^ "Var" ^ string_of_int n ^ string_of_int (Env.getNumSymbols env))) (Utils.range 0 (List.length targs)) in
            let rv = (match varNames with
                | [] -> Expr_Tuple []
                | [name] -> Expr_Var(name)
                | names -> Expr_Tuple(List.map (fun n -> Expr_Var n) names)) in
            let localPrefix = fName ^ string_of_int (Env.getNumSymbols env) in

            (* Add return type variables *)
            (* For each of these, if one already exists, add it to a stack to be restored after *)
            let returnTypeStmts = 
                Env.addImplicitLevel env;
                List.concat (List.map2 (fun arg e -> 
                (try (Env.addImplicitValue env arg (Env.getVar loc env arg)) with EvalError _ -> ());
                match dis_expr loc env e with
                | (Result v, stmts) -> Env.addLocalVar loc env arg v; stmts
                | (Simplified _, stmts) -> stmts
            ) targs tes) in

            (* Create dummy variable to assign return to *)
            let resultSymbolDecl = (match rty with 
            | Some t ->
                let (t', tstmts) = dis_type loc env t in
                tstmts @ [
                Stmt_VarDeclsNoInit(
                    t',
                    varNames, 
                    Unknown
                )]
            | None ->
                [Stmt_VarDeclsNoInit(
                    Type_Constructor (Ident "ERRORType"),
                    varNames, 
                    Unknown
                )]) 
            in

            (* Add local parameters, avoiding name collisions *)
            (* Also print what the parameter refers to *)
            let localParamStmts = (List.concat ((Utils.map3 (fun (ty, _) arg ex -> 
                let (ex', stmts) = dis_expr loc env ex in 
                (match ex' with 
                | Result v -> Env.addLocalVar loc env (Ident (localPrefix ^ pprint_ident arg)) v 
                | Simplified ex'' -> Env.addLocalVar loc env (Ident (localPrefix ^ pprint_ident arg)) VUninitialized);
                let (ty', tystmts) = dis_type loc env ty in
                stmts @ tystmts @ [Stmt_VarDecl(ty', Ident (localPrefix ^ (pprint_ident arg)), to_expr ex', loc)])
            ) atys args es)) in

            let bodyStmts = (Env.addReturnSymbol env rv;
            Env.addLocalPrefix env localPrefix;
            dis_stmts env b) in

            (Simplified rv,
            let result = 
                returnTypeStmts @ resultSymbolDecl @ localParamStmts @ bodyStmts
            in
                Env.removeReturnSymbol env;
                Env.removeLocalPrefix env;
                (* Restore implicit values *)
                List.iter (fun (arg, v) -> Env.addLocalVar loc env arg v) (Env.getImplicitLevel env);
                result
            )
        | None ->
            let (tes', tesstmts) = dis_exprs loc env tes in
            let (es', esstmts) = dis_exprs loc env es in
            (match eval_prim 
                (name_of_FIdent f) 
                (List.map (fun v -> match v with | Result v -> v | Simplified _ -> VUninitialized) tes') 
                (List.map (fun v -> match v with | Result v -> v | Simplified _ -> VUninitialized) es') with
            | Some VUninitialized -> (Simplified (Expr_TApply(f, List.map to_expr tes', List.map to_expr es')), tesstmts @ esstmts)
            | Some v -> (Result v, tesstmts @ esstmts)
            | None -> (Simplified (Expr_TApply(f, List.map to_expr tes', List.map to_expr es'))), tesstmts @ esstmts))

(** Dissassemble expression. This should never return Result VUninitialized *)
and dis_expr (loc: l) (env: Env.t) (x: AST.expr): (result_or_simplified * stmt list) =
    match x with
    | Expr_If(c, t, els, e) ->
        let rec eval_if xs d = match xs with
            | [] -> dis_expr loc env d
            (* If we cannot evaluate the condition, print the whole statement *)
            | AST.E_Elsif_Cond (cond, b)::xs' -> match dis_expr loc env cond with
                | (Result v, stmts) ->
                    if to_bool loc v then
                        (* Just print this branch *)
                        concat_stmts stmts (dis_expr loc env b)
                    else
                        (* Print whatever the rest of the branches turn out to be *)
                        concat_stmts stmts (eval_if xs' d)
                | (Simplified cond', stmts) -> 
                    let (b', bstmts) = dis_expr loc env b in
                    let (els', elsstmts) = dis_if_expr_no_remove loc env els in
                    let (e', estmts) = dis_expr loc env e in
                    (Simplified (Expr_If(
                        cond', 
                        to_expr (b'),
                        els',
                        to_expr (e')
                    )), stmts @ bstmts @ elsstmts @ estmts)
        in
        eval_if (E_Elsif_Cond(c, t)::els) e
    (* NOTE: This does not consider early returns currently. It also doesn't handle recursive calls *)
    | Expr_TApply(f, tes, es) ->
        dis_fun loc env f tes es
    | Expr_Var id ->
        (try 
            (match (Env.getVar loc env (Ident ((Env.getLocalPrefix loc env) ^ pprint_ident id))) with 
            | VUninitialized -> (Simplified (Expr_Var(Ident ((Env.getLocalPrefix loc env) ^ pprint_ident id))), [])
            | v -> (Result v, []))
        with EvalError _ ->
            (try 
                (match (Env.getVar loc env id) with 
                | VUninitialized -> (Simplified x , [])
                | v -> (Result v, []))
            with EvalError _ -> (Simplified x, [])))
    | Expr_In(e, p) ->
        (match dis_expr loc env e with
        | (Result v, stmts) -> (Result (from_bool (eval_pattern loc env v p)), stmts)
        | (Simplified _, stmts) -> (Simplified x, stmts))
    | Expr_Slices(e, ss) ->
        let transformedSlicesStmts = List.map (fun s -> dis_slice loc env s) ss in
        let transformedSlices = List.map (fun (sl, _) -> sl) transformedSlicesStmts in
        let stmts1 = List.concat (List.map (fun (_, s) -> s) transformedSlicesStmts) in
        (match dis_expr loc env e with
        | (Result v, stmts2) ->
            if List.exists (fun ts -> match ts with (Simplified _, _) -> true | (_, Simplified _) -> true | (_, _) -> false) transformedSlices then
                (Simplified (Expr_Slices(to_expr (Result v), List.map (fun (i, w) -> Slice_HiLo(to_expr i, to_expr w)) transformedSlices)), stmts1 @ stmts2)
            else
                let vs = List.map (fun s -> 
                    (match s with
                    | (Result v1, Result v2) -> extract_bits loc v v1 v2
                    | _ -> raise (EvalError (loc, "Unreachable: Shouldn't have expression in bit slice\n")))
                    ) transformedSlices in
                    (Result (eval_concat loc vs), stmts1 @ stmts2)
        | (Simplified e', stmts2) -> 
            (Simplified (Expr_Slices(e', List.map2 (fun (i, w) s ->
                (match s with
                | Slice_Single _ -> Slice_Single(to_expr i)
                | Slice_HiLo _ -> Slice_HiLo(to_expr i, to_expr w)
                | Slice_LoWd _ -> Slice_LoWd(to_expr i, to_expr w)
                )
            ) transformedSlices ss)), stmts1 @ stmts2))
    | Expr_Tuple(es) ->
        let transformedExprsStmts = List.map (dis_expr loc env) es in
        let transformedExprs = List.map (fun (e, _) -> e) transformedExprsStmts in
        let stmts = List.concat (List.map (fun (_, s) -> s) transformedExprsStmts) in
        if contains_expr transformedExprs then
            (Simplified (Expr_Tuple(List.map to_expr transformedExprs)), stmts)
        else
            (Result (VTuple (List.map to_value transformedExprs)), stmts)
    | Expr_Array(a, i) ->
        let (a', stmt1) = dis_expr loc env a in
        let (i', stmt2) = dis_expr loc env i in
        if is_expr a' || is_expr i' then
            (Simplified (Expr_Array(a, to_expr i')), stmt1 @ stmt2)
        else
            (match (get_array loc (to_value a') (to_value i')) with
            | VUninitialized -> (Simplified (Expr_Array(a, to_expr i')), stmt1 @ stmt2)
            | v -> (Result v, stmt1 @ stmt2))
    | x -> try (match eval_expr loc env x with VUninitialized -> (Simplified x, []) | v -> (Result v, [])) with EvalError (loc, message) -> (Simplified x, [])

(** Evaluate and simplify guards and bodies of an elseif chain, without removing branches *)
and dis_if_expr_no_remove (loc: l) (env: Env.t) (xs: e_elsif list): e_elsif list * stmt list = 
    match xs with
    | [] -> ([], [])
    | (AST.E_Elsif_Cond (cond, b)::xs') ->
        let (cond', condstmts) = dis_expr loc env cond in
        let (b', bstmts) = dis_expr loc env b in
        let (els', elsstmts) = dis_if_expr_no_remove loc env xs' in
        (AST.E_Elsif_Cond(
            to_expr (cond'), 
            to_expr (b')
        ) :: (els'), condstmts @ bstmts @ elsstmts)

and dis_lexpr (loc: l) (env: Env.t) (x: AST.lexpr) (r: result_or_simplified): stmt list =
    match x with
    | LExpr_Write(setter, tes, es) ->
        let tvsExprsStmts = List.map (dis_expr loc env) tes in
        let tvs = List.map (fun (e, _) -> e) tvsExprsStmts in
        let tvsStmts = List.map (fun (_, s) -> s) tvsExprsStmts in
        let vsExprsStmts = List.map (dis_expr loc env) es in
        let vs = List.map (fun (e, _) -> e) vsExprsStmts in
        let vsStmts = List.map (fun (_, s) -> s) vsExprsStmts in
        if contains_expr tvs || contains_expr vs || contains_expr [r] then begin
            (List.concat tvsStmts) @ (List.concat vsStmts) @ [Stmt_Assign(LExpr_Write(setter, List.map to_expr tvs, List.map to_expr vs), to_expr r, loc)]
        end else begin
            eval_proccall 
            loc 
            env 
            setter 
            (List.map (to_value) tvs) 
            ((List.map to_value vs) @ [to_value r]);
            [] 
        end    
    | LExpr_Var(v) -> 
        (match r with
        | Result r' -> 
            Env.setVar loc env v r'; []
        | Simplified e -> 
            Env.setVar loc env v VUninitialized;
            [Stmt_Assign(LExpr_Var(v), e, loc)])
    | _ -> try (eval_lexpr loc env x (to_value r); []) with EvalError _ ->
        [Stmt_Assign(x, to_expr r, loc)]

(** Dissassemble list of statements *)
and dis_stmts (env: Env.t) (xs: AST.stmt list): stmt list =
    List.concat (List.map (dis_stmt env) xs)

(** Evaluate and simplify guards and bodies of an elseif chain, without removing branches *)
and dis_if_stmt_no_remove (loc: l) (env: Env.t) (xs: s_elsif list): s_elsif list * stmt list = 
    match xs with
    | [] -> ([], [])
    | (AST.S_Elsif_Cond (cond, b)::xs') ->
        let (cond', condstmts) = dis_expr loc env cond in
        let (els', elsstmts) = dis_if_stmt_no_remove loc env xs' in
        (AST.S_Elsif_Cond(to_expr cond', dis_stmts env b)::(els'), condstmts @ elsstmts)

(** Disassemble statement *)
and dis_stmt (env: Env.t) (x: AST.stmt): stmt list =
    (match x with
    | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        let vs' = try (List.map (fun v -> Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v))) vs) with EvalError _ -> vs in
        List.iter (fun v -> Env.addLocalVar loc env v (mk_uninitialized loc env ty)) vs';
        let (ty', stmts) = dis_type loc env ty in
        stmts @ [Stmt_VarDeclsNoInit(ty', vs', loc)]
    | Stmt_VarDecl(ty, v, i, loc) ->
        let v' = try Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v)) with EvalError _ -> v in
        (match dis_expr loc env i with
        | (Result i', stmts) -> Env.addLocalVar loc env v' i'; stmts
        | (Simplified ex, stmts1) ->
            Env.addLocalVar loc env v' VUninitialized;
            let (ty', stmts2) = dis_type loc env ty in
            stmts1 @ stmts2 @ [Stmt_VarDecl(ty', v', ex, loc)]
        )
    | Stmt_ConstDecl(ty, v, i, loc) ->
        let v' = try Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v)) with EvalError _ -> v in
        (match dis_expr loc env i with
        | (Result i', stmts) -> Env.addLocalConst loc env v' i'; stmts
        | (Simplified ex, stmts1) -> 
            (* Declare constant with uninitialized value and just use symbolically *)
            Env.addLocalConst loc env v' VUninitialized;
            let (ty', stmts2) = dis_type loc env ty in
            stmts1 @ stmts2 @ [Stmt_ConstDecl(ty', v', ex, loc)]
        )
    | Stmt_Assign(l, r, loc) ->
        let (r', stmts) = dis_expr loc env r in
        stmts @ (dis_lexpr loc env l r')
    | Stmt_If(c, t, els, e, loc) ->
        let rec eval_if xs d = match xs with
            | [] -> dis_stmts env e
            | AST.S_Elsif_Cond (cond, b)::xs' ->
                (match dis_expr loc env cond with
                | (Result v, stmts) -> 
                    if to_bool loc v then
                        (* Just print this branch *)
                        stmts @ (dis_stmts env b)
                    else
                        (* Print whatever the rest of the branches turn out to be *)
                        stmts @ (eval_if xs' d)
                (* We have to print out all branches now because we don't know
                   whether or not this one is true. We can still simplify 
                   guards and bodies though *)
                | (Simplified e', stmts) -> 
                    let (els', elsstmts) = dis_if_stmt_no_remove loc env els in
                    stmts @ elsstmts @ [Stmt_If(e', dis_stmts env t, els', dis_stmts env e, loc)]
                )
        in
        eval_if (S_Elsif_Cond(c, t)::els) e
    | Stmt_FunReturn(e, loc) ->
        let (e', stmts) = dis_expr loc env e in
        stmts @ [Stmt_Assign((match (Env.getReturnSymbol loc env) with Expr_Var(i) -> LExpr_Var(i) | _ -> raise (EvalError (loc, "TODO"))), to_expr e', loc)]
    | Stmt_Assert(e, loc) ->
        (match dis_expr loc env e with 
        | (Result v, stmts) -> if not (to_bool loc v) then
            raise (EvalError (loc, "assertion failure")); stmts
        | (Simplified e', stmts) ->
            stmts @ [Stmt_Assert(e', loc)]
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
        (match dis_expr loc env e with
        | (Result v, stmts) -> stmts @ (eval v alts)
        | (Simplified _, stmts) ->
            (* TODO simplify cases individually*)
            stmts @ [x]
        ))
    | x -> [x]
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
                    List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) (dis_stmts env exec);
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
