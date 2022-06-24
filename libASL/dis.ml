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
    if v == ExprValue VUninitialized then ex else v

(** Convert value to a simple expression containing that value, so we can
    print it or use it symbolically *)
let value_to_expr (v: valueOrExpr): AST.expr = 
    match v with
    | ExprValue x ->
        (match x with 
        | VBool b -> Expr_LitInt(if b then "1" else "0")
        (* TODO: definitely get rid of this. Should convert every case *)
        | x -> Expr_LitInt("0")
        )
    | Expr x -> x


let rec dis_expr (loc: l) (env: Env.t) (x: AST.expr): valueOrExpr =
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
                    value_to_expr (remove_uninitialized (dis_expr loc env b) (Expr b))
                ))
        in
        eval_if (E_Elsif_Cond(c, t)::els) e
    (* NOTE: This does not consider early returns currently. It also doesn't handle recursive calls *)
    | Expr_TApply(f, tes, es) ->
        (try
            (let (targs, args, loc, b) = Env.getFun loc env f in
                (* Initialise all the dummy variables used for return variables *)
                let fName = match f with
                | Ident t -> t
                | FIdent (t, _) -> t in
                let ds = Stmt_VarDeclsNoInit(
                    Type_Constructor (Ident "TODOType"), 
                    List.map (fun n -> Ident (fName ^ "Var" ^ string_of_int n)) (Utils.range 0 (List.length targs - 1)), 
                    Unknown
                ) in Printf.printf "%s\n" (pp_stmt ds);

                (let rv = (
                    if List.length tes == 1 then 
                        Expr_Var(Ident (fName ^ "Var0"))
                    else
                        Expr_Tuple(List.map (fun n -> Expr_Var(Ident (fName ^ "Var" ^ string_of_int n))) (Utils.range 0 (List.length tes - 1))))
                in

                (* print out the body *)
                Env.addReturnSymbol env rv;
                dis_stmts env b;
                Expr rv))

                (* resolve to a singular dummy variable or a tuple of the dummy variables for assignment *)
                
        (* Use this to identify statements not being partially evaluated as expected *)
        (* with EvalError (loc, message) -> Printf.printf "ERROR: %s\n" message; Expr x)  *)
        with EvalError (loc, message) -> Expr x)
    | x -> (try (ExprValue (eval_expr loc env x)) with EvalError _ -> Expr x)
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
        Printf.printf "%s\n" (pp_stmt x);
        List.iter (fun v -> Env.addLocalVar loc env v (mk_uninitialized loc env ty)) vs
    | Stmt_VarDecl(ty, v, i, loc) ->
        (match dis_expr loc env i with
        | ExprValue i' -> Env.addLocalVar loc env v i'
        | Expr ex -> 
            (* Declare variable with uninitialized value and just use symbolically *)
            Printf.printf "%s %s = %s\n" (pp_type ty) (pprint_ident v) (pp_expr ex);
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
        Printf.printf "%s = %s\n" (pp_expr (Env.getReturnSymbol loc env)) (pp_expr e)
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
