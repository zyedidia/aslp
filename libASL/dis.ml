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
let rec value_to_expr (v: result_or_simplified): AST.expr = 
    match v with
    | Result x ->
        (match x with 
        | VBool b -> Expr_LitInt(if b then "1" else "0")
        | VEnum (id, n) -> Expr_LitInt(string_of_int n)
        | VInt n -> Expr_LitInt(string_of_int (Z.to_int n))
        | VReal n -> Expr_LitReal(string_of_float (Q.to_float n))
        | VBits {n; v} -> Expr_LitInt(string_of_int (Z.to_int v))
        | VString s -> Expr_LitString(s)
        | VTuple vs -> Expr_Tuple(List.map (fun v -> value_to_expr (Result v)) vs)
        (* TODO: definitely get rid of this. Should convert every case *)
        | x -> Expr_LitInt("0")
        )
    | Simplified x -> x

let rec dis_type (loc: l) (env: Env.t) (t: ty): ty =
    match t with
    | Type_Bits ex -> Type_Bits (match dis_expr loc env ex with | Result v -> value_to_expr (Result v) | Simplified x -> x)
    | Type_OfExpr ex -> Type_OfExpr (match dis_expr loc env ex with | Result v -> value_to_expr (Result v) | Simplified x -> x)
    | Type_Tuple tys -> Type_Tuple (List.map (dis_type loc env) tys)
    | t' -> t'

(** Evaluate list of expressions *)
and dis_exprs (loc: l) (env: Env.t) (xs: AST.expr list): result_or_simplified list =
    List.map (dis_expr loc env) xs

(** Evaluate bitslice bounds *)
and dis_slice (loc: l) (env: Env.t) (x: AST.slice): (result_or_simplified * result_or_simplified) =
    (match x with
    | Slice_Single(i) ->
            (dis_expr loc env i, Result (VInt Z.one))
    | Slice_HiLo(hi, lo) ->
            let hi' = dis_expr loc env hi in
            let lo' = dis_expr loc env lo in
            (match hi' with
            | Result vh -> (match lo' with
                | Result vl -> (Result vl, Result (eval_add_int loc (eval_sub_int loc vh vl) (VInt Z.one)))
                | Simplified el -> (Simplified el, Simplified (Expr_Binop(Expr_Binop(value_to_expr (Result vh), Binop_Minus, el), Binop_Plus, value_to_expr (Result (VInt Z.one))))))
            | Simplified eh -> (match lo' with
                | Result vl -> (Result vl, Simplified (Expr_Binop(Expr_Binop(eh, Binop_Minus, value_to_expr (Result vl)), Binop_Plus, value_to_expr (Result (VInt Z.one)))))
                | Simplified el -> (Simplified el, Simplified (Expr_Binop(Expr_Binop(eh, Binop_Minus, el), Binop_Plus, value_to_expr (Result (VInt Z.one)))))))
    | Slice_LoWd(lo, wd) ->
            let lo' = dis_expr loc env lo in
            let wd' = dis_expr loc env wd in
            (lo', wd')
    )

and dis_fun (loc: l) (env: Env.t) (f: ident) (tes: AST.expr list) (es: AST.expr list): result_or_simplified =
    if name_of_FIdent f = "and_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> (match dis_expr loc env x with
            | Result v -> if to_bool loc v then dis_expr loc env y else Result (from_bool false)
            | Simplified e -> Simplified (Expr_TApply(f, tes, [e; value_to_expr (dis_expr loc env y)])))
        | _ ->
            raise (EvalError (loc, "malformed and_bool expression"))
        )
    end else if name_of_FIdent f = "or_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> (match dis_expr loc env x with
            | Result v -> if to_bool loc v then Result (from_bool true) else dis_expr loc env y
            | Simplified e -> Simplified (Expr_TApply(f, tes, [e; value_to_expr (dis_expr loc env y)])))
        | _ ->
            raise (EvalError (loc, "malformed or_bool expression"))
        )
    end else if name_of_FIdent f = "implies_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> (match dis_expr loc env x with
            | Result v -> if to_bool loc v then dis_expr loc env y else Result (from_bool true)
            | Simplified e -> Simplified (Expr_TApply(f, tes, [e; value_to_expr (dis_expr loc env y)])))
        | _ ->
            raise (EvalError (loc, "malformed implies_bool expression"))
        )
    end else (match (try (Some (Env.getFun loc env f)) with EvalError _ -> None) with
        | Some (rty, atys, targs, args, loc, b) ->
            (* Add return type variables *)
            List.iter2 (fun arg e -> match dis_expr loc env e with
                | Result v -> Env.addLocalVar loc env arg v
                | Simplified _ -> ()
            ) targs tes;

            (let fName = name_of_FIdent f in
            let varNames = List.map (fun n -> Ident (fName ^ "Var" ^ string_of_int n ^ string_of_int (Env.getNumSymbols env))) (Utils.range 0 (List.length targs)) in
            let ds = Stmt_VarDeclsNoInit(
                (match rty with Some t -> dis_type loc env t | None -> Type_Constructor (Ident "ERRORType")),
                varNames, 
                Unknown
            ) in Printf.printf "%s\n" (pp_stmt ds);
            let rv = (match varNames with
                | [] -> Expr_Tuple []
                | [name] -> Expr_Var(name)
                | names -> Expr_Tuple(List.map (fun n -> Expr_Var n) names)) in
            let localPrefix = fName ^ string_of_int (Env.getNumSymbols env) in

            (* Add local parameters, avoiding name collisions *)
            (* Also print what the parameter refers to *)
            Utils.iter3 (fun (ty, _) arg ex -> 
                let ex' = dis_expr loc env ex in 
                (match ex' with 
                | Result v -> Env.addLocalVar loc env (Ident (localPrefix ^ pprint_ident arg)) v 
                | Simplified ex'' -> Env.addLocalVar loc env (Ident (localPrefix ^ pprint_ident arg)) VUninitialized);
                Printf.printf "%s %s = %s\n" (pp_type (dis_type loc env ty)) (localPrefix ^ (pprint_ident arg)) (pp_result_or_simplified ex')
            ) atys args es;

            (* print out the body *)
            Env.addReturnSymbol env rv;
            Env.addLocalPrefix env localPrefix;
            dis_stmts env b;
            Env.removeReturnSymbol env;
            Env.removeLocalPrefix env;
            Simplified rv)
        | None ->
            let tes' = dis_exprs loc env tes in
            let es' = dis_exprs loc env es in
            (match eval_prim 
                (name_of_FIdent f) 
                (List.map (fun v -> match v with | Result v -> v | Simplified _ -> VUninitialized) tes') 
                (List.map (fun v -> match v with | Result v -> v | Simplified _ -> VUninitialized) es') with
            | Some VUninitialized -> Simplified (Expr_TApply(f, List.map value_to_expr tes', List.map value_to_expr es'))
            | Some v -> Result v
            | None -> Simplified (Expr_TApply(f, List.map value_to_expr tes', List.map value_to_expr es'))))

(** This should never return Result VUninitialized *)
and dis_expr (loc: l) (env: Env.t) (x: AST.expr): result_or_simplified =
    match x with
    | Expr_If(c, t, els, e) ->
        let rec eval_if xs d = match xs with
            | [] -> dis_expr loc env d
            (* If we cannot evaluate the condition, print the whole statement *)
            | AST.E_Elsif_Cond (cond, b)::xs' -> match dis_expr loc env cond with
                | Result v -> 
                    if to_bool loc v then
                        (* Just print this branch *)
                        dis_expr loc env b
                    else
                        (* Print whatever the rest of the branches turn out to be *)
                        eval_if xs' d
                | Simplified e' -> 
                    Simplified (Expr_If(
                        e', 
                        value_to_expr (dis_expr loc env b),
                        dis_if_expr_no_remove loc env els,
                        value_to_expr (dis_expr loc env e)
                    ))
        in
        eval_if (E_Elsif_Cond(c, t)::els) e
    (* NOTE: This does not consider early returns currently. It also doesn't handle recursive calls *)
    | Expr_TApply(f, tes, es) ->
        dis_fun loc env f tes es
    | Expr_Var id ->
        (try 
            (match (Env.getVar loc env (Ident ((Env.getLocalPrefix loc env) ^ pprint_ident id))) with 
            | VUninitialized -> Simplified (Expr_Var(Ident ((Env.getLocalPrefix loc env) ^ pprint_ident id))) 
            | v -> Result v)
        with EvalError _ ->
            (try 
                (match (Env.getVar loc env id) with 
                | VUninitialized -> Simplified x 
                | v -> Result v)
            with EvalError _ -> Simplified x))
    | Expr_In(e, p) ->
        (match dis_expr loc env e with
        | Result v -> Result (from_bool (eval_pattern loc env v p))
        | Simplified _ -> Simplified x)
    | Expr_Slices(e, ss) ->
        let transformedSlices = List.map (fun s -> dis_slice loc env s) ss in
        (match dis_expr loc env e with
        | Result v ->
            if List.exists (fun ts -> match ts with (Simplified _, _) -> true | (_, Simplified _) -> true | (_, _) -> false) transformedSlices then
                Simplified (Expr_Slices(value_to_expr (Result v), List.map (fun (i, w) -> Slice_HiLo(value_to_expr i, value_to_expr w)) transformedSlices))
            else
                let vs = List.map (fun s -> 
                    (match s with
                    | (Result v1, Result v2) -> extract_bits loc v v1 v2
                    | _ -> raise (EvalError (loc, "Unreachable: Shouldn't have expression in bit slice\n")))
                    ) transformedSlices in
                    Result (eval_concat loc vs)
        | Simplified e' -> 
            Simplified (Expr_Slices(e', List.map2 (fun (i, w) s ->
                (match s with
                | Slice_Single _ -> Slice_Single(value_to_expr i)
                | Slice_HiLo _ -> Slice_HiLo(value_to_expr i, value_to_expr w)
                | Slice_LoWd _ -> Slice_LoWd(value_to_expr i, value_to_expr w)
                )
            ) transformedSlices ss)))
    | Expr_Tuple(es) ->
        let transformedExprs = List.map (dis_expr loc env) es in
        if List.exists (fun e -> match e with Result _ -> false | Simplified _ -> true) transformedExprs then
            Simplified (Expr_Tuple(List.map value_to_expr transformedExprs))
        else
            Result (VTuple (List.map (fun te -> match te with Result v -> v | Simplified _ -> raise (EvalError (loc, "Unreachable: cannot have expr in list"))) transformedExprs))
    | x -> try (match eval_expr loc env x with VUninitialized -> Simplified x | v -> Result v) with EvalError (loc, message) -> Simplified x

(** Evaluate and simplify guards and bodies of an elseif chain, without removing branches *)
and dis_if_expr_no_remove (loc: l) (env: Env.t) (xs: e_elsif list): e_elsif list = 
    match xs with
    | [] -> []
    | (AST.E_Elsif_Cond (cond, b)::xs') ->
        AST.E_Elsif_Cond(
            value_to_expr (dis_expr loc env cond), 
            value_to_expr (dis_expr loc env b)
        ) :: (dis_if_expr_no_remove loc env xs')

and dis_lexpr (loc: l) (env: Env.t) (x: AST.lexpr) (r: result_or_simplified): unit =
    match x with
    | LExpr_Write(setter, tes, es) ->
        let contains_expr = List.exists (fun a -> match a with Result _ -> false | Simplified _ -> true) in
        let tvs = List.map (dis_expr loc env) tes in
        let vs = List.map (dis_expr loc env) es in
        if contains_expr tvs || contains_expr vs || contains_expr [r] then begin
            Printf.printf "__write %s {{ " (pprint_ident setter);
            List.iter (fun tv -> Printf.printf "%s " (pp_result_or_simplified tv)) tvs;
            Printf.printf "}} [ ";
            List.iter (fun v -> Printf.printf "%s " (pp_result_or_simplified v)) vs;
            Printf.printf "] = %s\n" (pp_result_or_simplified r)
        end else
            eval_proccall 
            loc 
            env 
            setter 
            (List.map (fun a -> match a with Result v -> v | Simplified _ -> raise (EvalError (loc, "Unreachable"))) tvs) 
            (List.append (List.map (fun a -> match a with Result v -> v | Simplified _ -> raise (EvalError (loc, "Unreachable"))) vs) 
                [match r with Result v -> v | Simplified _ -> raise (EvalError (loc, "Unreachable"))])
    | LExpr_Var(v) -> 
        (match r with
        | Result r' -> 
            Env.setVar loc env v r'
        | Simplified e -> 
            Env.setVar loc env v VUninitialized; 
            Printf.printf "%s = %s\n" (pprint_ident v) (pp_expr e))
    | _ -> try (eval_lexpr loc env x (match r with Result v -> v | Simplified _ -> raise (EvalError (loc, "Unable to evaluate")))) with EvalError _ -> Printf.printf "%s\n" (pp_lexpr x)

(** Dissassemble list of statements *)
and dis_stmts (env: Env.t) (xs: AST.stmt list): unit =
    Env.nest (fun env' -> List.iter (dis_stmt env') xs) env

(** Evaluate and simplify guards and bodies of an elseif chain, without removing branches *)
and dis_if_stmt_no_remove (loc: l) (env: Env.t) (xs: s_elsif list): unit = 
    match xs with
    | [] -> ()
    | (AST.S_Elsif_Cond (cond, b)::xs') ->
        Printf.printf "} else if %s then {\n" (pp_expr cond);
        dis_stmts env b

(** Disassemble statement *)
and dis_stmt (env: Env.t) (x: AST.stmt): unit =
    (match x with
    | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        Printf.printf "%s\n" (pp_stmt (Stmt_VarDeclsNoInit(dis_type loc env ty, vs, loc)));
        List.iter (fun v -> Env.addLocalVar loc env v (mk_uninitialized loc env ty)) vs
    | Stmt_VarDecl(ty, v, i, loc) ->
        (match dis_expr loc env i with
        | Result i' -> Env.addLocalVar loc env v i'
        | Simplified ex -> 
            (* Declare variable with uninitialized value and just use symbolically *)
            Printf.printf "%s %s = %s\n" (pp_type (dis_type loc env ty)) (pprint_ident v) (pp_expr ex);
            Env.addLocalVar loc env v VUninitialized
        )
    | Stmt_ConstDecl(ty, v, i, loc) ->
        (match dis_expr loc env i with
        | Result i' -> Env.addLocalConst loc env v i'
        | Simplified ex -> 
            (* Declare constant with uninitialized value and just use symbolically *)
            Printf.printf "%s\n" (pp_stmt x);
            Env.addLocalConst loc env v (mk_uninitialized loc env ty)
        )
    | Stmt_Assign(l, r, loc) ->
        dis_lexpr loc env l (dis_expr loc env r)
    | Stmt_If(c, t, els, e, loc) ->
        let rec eval_if xs d = match xs with
            | [] -> dis_stmts env e
            | AST.S_Elsif_Cond (cond, b)::xs' ->
                (match dis_expr loc env cond with
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
                | Simplified e' -> 
                    Printf.printf "if ";
                    Printf.printf "%s" (pp_expr e');
                    Printf.printf " then {\n";
                    dis_stmts env t;
                    dis_if_stmt_no_remove loc env els;
                    Printf.printf "} else {\n";
                    dis_stmts env e;
                    Printf.printf "}\n"
                )
        in
        eval_if (S_Elsif_Cond(c, t)::els) e
    | Stmt_FunReturn(e, loc) ->
        Printf.printf "%s = %s\n" (pp_expr (Env.getReturnSymbol loc env)) (match dis_expr loc env e with Result v -> pp_value v | Simplified e' -> pp_expr e')
    | Stmt_Assert(e, loc) ->
        (match dis_expr loc env e with 
        | Result v -> if not (to_bool loc v) then
            raise (EvalError (loc, "assertion failure"))
        | Simplified e' -> Printf.printf "assert %s\n"(pp_expr e')
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
        | Result v -> eval v alts
        | Simplified _ -> Printf.printf "%s\n" (pp_stmt x)))
    | x -> Printf.printf "%s\n" (pp_stmt x)
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
                    dis_stmts env exec;
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
