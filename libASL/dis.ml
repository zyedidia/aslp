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

type valueOrExpr =
    | ExprValue of value
    | Expr of AST.expr

(** Return the original expression if the evaluation is undefined *)
let remove_uninitialized (v: valueOrExpr) (ex: valueOrExpr): valueOrExpr =
    match v with
    | ExprValue VUninitialized -> ex 
    | _ -> v

let pp_valueOrExpr (v: valueOrExpr) = 
    match v with
    | ExprValue v' -> pp_value v'
    | Expr v' -> pp_expr v'

(** Convert value to a simple expression containing that value, so we can
    print it or use it symbolically *)
let rec value_to_expr (v: valueOrExpr): AST.expr = 
    match v with
    | ExprValue x ->
        (match x with 
        | VBool b -> Expr_LitInt(if b then "1" else "0")
        | VEnum (id, n) -> Expr_LitInt(string_of_int n)
        | VInt n -> Expr_LitInt(string_of_int (Z.to_int n))
        | VReal n -> Expr_LitReal(string_of_float (Q.to_float n))
        | VBits {n; v} -> Expr_LitInt(string_of_int (Z.to_int v))
        | VString s -> Expr_LitString(s)
        | VTuple vs -> Expr_Tuple(List.map (fun v -> value_to_expr (ExprValue v)) vs)
        (* TODO: definitely get rid of this. Should convert every case *)
        | x -> Expr_LitInt("0")
        )
    | Expr x -> x

let rec dis_type (loc: l) (env: Env.t) (t: ty): ty =
    match t with
    | Type_Bits ex -> Type_Bits (match dis_expr loc env ex with | ExprValue v -> value_to_expr (ExprValue v) | Expr x -> x)
    | t' -> t'

(** Evaluate bitslice bounds *)
and dis_slice (loc: l) (env: Env.t) (x: AST.slice): (valueOrExpr * valueOrExpr) =
    (match x with
    | Slice_Single(i) ->
            (dis_expr loc env i, ExprValue (VInt Z.one))
    | Slice_HiLo(hi, lo) ->
            (* TODO: partially evaluate this case. Tricky because not sure what to have for wd' in failure case. This can currently throw an exception *)
            let hi' = eval_expr loc env hi in
            let lo' = eval_expr loc env lo in
            let wd' = eval_add_int loc (eval_sub_int loc hi' lo') (VInt Z.one) in
            (ExprValue lo', ExprValue wd')
    | Slice_LoWd(lo, wd) ->
            let lo' = dis_expr loc env lo in
            let wd' = dis_expr loc env wd in
            (lo', wd')
    )

and slices_contain_expr xs: bool =
    match xs with
    | [] -> false
    | ((Expr _, _)::xs') -> true
    | ((_, Expr _)::xs') -> true
    | ((_, _)::xs') -> true

(** Evaluate list of expressions *)
and dis_exprs (loc: l) (env: Env.t) (xs: AST.expr list): valueOrExpr list =
    List.map (dis_expr loc env) xs

and dis_expr (loc: l) (env: Env.t) (x: AST.expr): valueOrExpr =
    (match x with
    | Expr_If(c, t, els, e) ->
        let rec eval_if xs d = match xs with
            (* if the result of dis_expr is Uninit. then we want to return the entire expr*)
            | [] -> remove_uninitialized (dis_expr loc env d) (Expr d)
            (* If we cannot evaluate the condition, print the whole statement *)
            | AST.E_Elsif_Cond (cond, b)::xs' ->
                match remove_uninitialized (dis_expr loc env cond) (Expr cond) with
                | ExprValue v -> 
                    if to_bool loc v then
                        (* Just print this branch *)
                        dis_expr loc env b
                    else
                        (* Print whatever the rest of the branches turn out to be *)
                        eval_if xs' d
                (* We have to print out all branches now because we don't know
                   whether or not this one is true. We can still simplify 
                   guards and bodies though *)
                | Expr ex -> Expr (Expr_If(
                    ex, 
                    value_to_expr (remove_uninitialized (dis_expr loc env b) (Expr b)), 
                    dis_if_expr_no_remove loc env els,
                    value_to_expr (remove_uninitialized (dis_expr loc env e) (Expr e))
                ))
        in
        eval_if (E_Elsif_Cond(c, t)::els) e
    (* NOTE: This does not consider early returns currently. It also doesn't handle recursive calls *)
    | Expr_TApply(f, tes, es) ->
        if name_of_FIdent f = "and_bool" then begin
            (match (tes, es) with
            | ([], [x; y]) ->
                (match dis_expr loc env x with
                | ExprValue v -> if to_bool loc v then dis_expr loc env y else ExprValue (from_bool false)
                | Expr e -> Expr x)
            | _ ->
                raise (EvalError (loc, "malformed and_bool expression "
                ^ Utils.to_string (PP.pp_expr x)))
            )
        end else if name_of_FIdent f = "or_bool" then begin
            (match (tes, es) with
            | ([], [x; y]) ->
                (match dis_expr loc env x with
                | ExprValue v -> if to_bool loc v then ExprValue (from_bool true) else dis_expr loc env y
                | Expr e -> Expr x)
            | _ ->
                raise (EvalError (loc, "malformed or_bool expression "
                ^ Utils.to_string (PP.pp_expr x)))
            )
        end else if name_of_FIdent f = "implies_bool" then begin
            (match (tes, es) with
            | ([], [x; y]) ->
                (match dis_expr loc env x with
                | ExprValue v -> if to_bool loc v then dis_expr loc env y else ExprValue (from_bool true)
                | Expr e -> Expr x)
            | _ ->
                raise (EvalError (loc, "malformed implies_bool expression "
                ^ Utils.to_string (PP.pp_expr x)))
            )
        end else (match (try (Some (Env.getFun loc env f)) with EvalError _ -> None) with
            | Some (targs, args, loc, b) -> 
                List.iter2 (fun arg e -> 
                    match dis_expr loc env e with
                    | ExprValue v -> Env.addLocalVar loc env arg v
                    | Expr _ -> ()
                ) targs tes;
                (let fName = match f with
                | Ident t -> t
                | FIdent (t, _) -> t in
                (* TODO: Pass the typing information through when a function is declared and extract it here. 
                   For now, just won't print return symbol declarations *)
                (* let varTypes = List.map (fun n -> try (Env.getVar loc env n) with EvalError _ -> VUninitialized) targs in *)
                let varNames = List.map (fun n -> Ident (fName ^ "Var" ^ string_of_int n ^ string_of_int (Env.getNumSymbols env))) (Utils.range 0 (List.length targs - 1)) in
                (* let ds = Stmt_VarDeclsNoInit(
                    (match varTypes with | [] -> Type_Constructor (Ident "ERRORType") | (x::xs) -> Type_Bits (value_to_expr (ExprValue x))), 
                    varNames, 
                    Unknown
                ) in Printf.printf "%s\n" (pp_stmt ds); *)

                (let rv = (
                    match varNames with
                    | [] -> Expr_Tuple []
                    | [name] -> Expr_Var(name)
                    | names -> Expr_Tuple(List.map (fun n -> Expr_Var n) names))
                in

                (* Add local parameters *)
                (* Also print what the parameter refers to *)
                (* TODO: fix naming conflicts if this function is called twice in one expression *)
                List.iter2 (fun arg ex -> 
                    let ex' = dis_expr loc env ex in 
                    (match ex' with 
                    | ExprValue v -> Env.addLocalVar loc env arg v 
                    | Expr _ -> ());
                    Printf.printf "%s = %s\n" (pprint_ident arg) (pp_valueOrExpr (remove_uninitialized ex' (Expr ex)))) args es;

                (* print out the body *)
                Env.addReturnSymbol env rv;
                dis_stmts env b;
                Env.removeReturnSymbol env;
                Expr rv))
            | None ->
                let tes' = dis_exprs loc env tes in
                let es' = dis_exprs loc env es in
                (match eval_prim 
                    (name_of_FIdent f) 
                    (List.map (fun v -> match v with | ExprValue v -> v | Expr _ -> VUninitialized) tes') 
                    (List.map (fun v -> match v with | ExprValue v -> v | Expr _ -> VUninitialized) es') with
                | Some v -> ExprValue v
                | None -> Expr (Expr_TApply(f, List.map value_to_expr tes', List.map value_to_expr es'))))
    | Expr_Var id ->
        (try (remove_uninitialized (ExprValue (Env.getVar loc env id)) (Expr (Expr_Var id))) with EvalError (loc, message) -> Expr x)
    | Expr_In(e, p) ->
        (match dis_expr loc env e with
        | ExprValue v -> ExprValue (from_bool (eval_pattern loc env v p))
        | Expr _ -> Expr x)
    | Expr_Slices(e, ss) ->
        let transformedSlices = List.map (fun s -> dis_slice loc env s) ss in
        (match dis_expr loc env e with
        | ExprValue v ->
            if not (slices_contain_expr transformedSlices) then
                let vs = List.map (fun s -> 
                    (match s with
                    | (ExprValue v1, ExprValue v2) -> extract_bits loc v v1 v2
                    | _ -> raise (EvalError (loc, "Shouldn't have expression in bit slice\n")))
                    ) transformedSlices in
                ExprValue (eval_concat loc vs)
                else
                    Expr (Expr_Slices(value_to_expr (ExprValue v), List.map (fun (i, w) -> Slice_HiLo(value_to_expr i, value_to_expr w)) transformedSlices))
        | Expr e' -> 
            Expr (Expr_Slices(e', List.map2 (fun (i, w) s ->
                (match s with
                | Slice_Single _ -> Slice_Single(value_to_expr i)
                | Slice_HiLo _ -> Slice_HiLo(value_to_expr i, value_to_expr w)
                | Slice_LoWd _ -> Slice_LoWd(value_to_expr i, value_to_expr w)
                )
            ) transformedSlices ss)))
    | x -> (try (remove_uninitialized (ExprValue (eval_expr loc env x)) (Expr x)) with EvalError (loc, message) -> Expr x)
    )

(** Evaluate and simplify guards and bodies of an elseif chain, without removing branches *)
and dis_if_expr_no_remove (loc: l) (env: Env.t) xs = 
    match xs with
    | [] -> []
    | (AST.E_Elsif_Cond (cond, b)::xs') ->
        AST.E_Elsif_Cond(
            value_to_expr (remove_uninitialized (dis_expr loc env cond) (Expr cond)), 
            value_to_expr (remove_uninitialized (dis_expr loc env b) (Expr b))
        ) :: (dis_if_expr_no_remove loc env xs')

(** Dissassemble list of statements *)
and dis_stmts (env: Env.t) (xs: AST.stmt list): unit =
    Env.nest (fun env' -> List.iter (dis_stmt env') xs) env

(** Disassemble statement *)
and dis_stmt (env: Env.t) (x: AST.stmt): unit =
    (match x with
    | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        Printf.printf "%s\n" (pp_stmt (Stmt_VarDeclsNoInit(dis_type loc env ty, vs, loc)));
        List.iter (fun v -> Env.addLocalVar loc env v (mk_uninitialized loc env ty)) vs
    | Stmt_VarDecl(ty, v, i, loc) ->
        (match dis_expr loc env i with
        | ExprValue i' -> Env.addLocalVar loc env v i'
        | Expr ex -> 
            (* Declare variable with uninitialized value and just use symbolically *)
            Printf.printf "%s %s = %s\n" (pp_type (dis_type loc env ty)) (pprint_ident v) (pp_expr ex);
            Env.addLocalVar loc env v VUninitialized
        )
    | Stmt_ConstDecl(ty, v, i, loc) ->
        (match dis_expr loc env i with
        | ExprValue i' -> Env.addLocalConst loc env v i'
        | Expr ex -> 
            (* Declare constant with uninitialized value and just use symbolically *)
            Printf.printf "%s\n" (pp_stmt x);
            Env.addLocalConst loc env v (mk_uninitialized loc env ty)
        )
    | Stmt_Assign(l, r, loc) ->
        (match dis_expr loc env r with
        (* TODO: handle left the same way we handle right. needs dis_lexpr *)
        | ExprValue r' -> 
            (try (eval_lexpr loc env l r') with EvalError _ -> Printf.printf "%s\n" (pp_stmt x))
        | Expr ex -> Printf.printf "%s = %s\n" (pp_lexpr l) (pp_expr ex);
        )
    | Stmt_If(c, t, els, e, loc) ->
        let rec eval_if xs d = match xs with
            | [] -> dis_stmts env e
            | AST.S_Elsif_Cond (cond, b)::xs' ->
                (match remove_uninitialized (dis_expr loc env cond) (Expr cond) with
                | ExprValue v -> 
                    if to_bool loc v then
                        (* Just print this branch *)
                        dis_stmts env b
                    else
                        (* Print whatever the rest of the branches turn out to be *)
                        eval_if xs' d
                (* We have to print out all branches now because we don't know
                   whether or not this one is true. We can still simplify 
                   guards and bodies though *)
                | Expr ex -> 
                    Printf.printf "if ";
                    Printf.printf "%s" (pp_expr ex);
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
        Printf.printf "%s = %s\n" (pp_expr (Env.getReturnSymbol loc env)) (match dis_expr loc env e with ExprValue v -> pp_value v | Expr ex -> pp_expr ex)
    | Stmt_Assert(e, loc) ->
        (match dis_expr loc env e with 
        | ExprValue v -> if not (to_bool loc v) then
            raise (EvalError (loc, "assertion failure"))
        | Expr x -> Printf.printf "assert %s\n"(pp_expr x)
        )
    | x -> Printf.printf "%s\n" (pp_stmt x)
    )

(** Evaluate and simplify guards and bodies of an elseif chain, without removing branches *)
and dis_if_stmt_no_remove (loc: l) (env: Env.t) xs = 
    match xs with
    | [] -> ()
    | (AST.S_Elsif_Cond (cond, b)::xs') ->
        Printf.printf "} else if %s then {\n" (pp_expr cond);
        dis_stmts env b

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
