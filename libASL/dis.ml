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

(** Dissassemble list of statements *)
let rec dis_stmts (env: Eval.Env.t) (xs: AST.stmt list):unit =
    Env.nest (fun env' -> List.iter (dis_stmt env') xs) env

(** Disassemble statement *)
and dis_stmt (env: Eval.Env.t) (x: AST.stmt): unit =
    (match x with
    | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        List.iter (fun v -> Env.addLocalVar loc env v (mk_uninitialized loc env ty)) vs
    | Stmt_VarDecl(ty, v, i, loc) ->
        let i' = (try (eval_expr loc env i) with EvalError _ -> VUninitialized) in
        if i' != VUninitialized then 
            Env.addLocalVar loc env v i'
        else 
            (* Declare variable with uninitialized value and just use symbolically *)
            Printf.printf "%s\n" (pp_stmt x);
            Env.addLocalVar loc env v (mk_uninitialized loc env ty)
    | Stmt_ConstDecl(ty, v, i, loc) ->
            let i' = eval_expr loc env i in
            Env.addLocalConst loc env v i'
    (* TODO: handle other statement types *)
    | Stmt_Assign(l, r, loc) ->
        (* TODO: create dis_expr which checks for unknown at every recursive call *)
        (match r with
        (* TODO: treat variable as symbolic from now on if unknown *)
        | Expr_Unknown(t) -> Printf.printf "%s\n" (pp_stmt x)
        | r -> let r' = try eval_expr loc env r with EvalError _ -> VUninitialized in 
            if r' != VUninitialized then
                eval_lexpr loc env l r'
            else
                Printf.printf "%s\n" (pp_stmt x)
        )
    | Stmt_If(c, t, els, e, loc) ->
            let rec eval css d =
                (match css with
                | [] -> dis_stmts env d
                | (S_Elsif_Cond(c, s) :: css') ->
                    (* TODO: only evaluate if expression only contains constants *)
                    if to_bool loc (eval_expr loc env c) then
                        dis_stmts env s
                    else
                        eval css' d
                )
            in
            eval (S_Elsif_Cond(c, t) :: els) e
    | x -> Printf.printf "%s\n" (pp_stmt x)
    )

(* Duplicate of eval_decode_case modified to print rather than eval *)
let rec dis_decode_case (loc: AST.l) (env: Eval.Env.t) (x: decode_case) (op: value): unit =
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
and dis_decode_alt (loc: AST.l) (env: Eval.Env.t) (DecoderAlt_Alt (ps, b)) (vs: value list) (op: value): bool =
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
