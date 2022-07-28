open Asl_utils

open AST

module RemoveUnused = struct

  module M = Rws.Make(struct
    type r = unit
    type w = stmt list
    type s = IdentSet.t

    let mappend = (@)
    let mempty = []
  end)

  include M.Let

  let rec remove_unused' (xs: stmt list): unit M.rws =
    M.traverse_ (fun stmt ->
      let* used = M.get in

      let include_stmt (s: stmt): unit M.rws =
        let* () = M.write [s] in
        M.modify (IdentSet.union (fv_stmt s)) in

      match stmt with
      | Stmt_VarDeclsNoInit(ty, vs, loc) ->
          let vs' = List.filter (fun i -> IdentSet.mem i used) vs in
          (match vs' with
          | [] -> M.unit
          | _ -> include_stmt (Stmt_VarDeclsNoInit(ty, vs', loc)))
      | Stmt_VarDecl(ty, v, i, loc) ->
          if IdentSet.mem v used
            then include_stmt stmt
            else M.unit
      | Stmt_ConstDecl(ty, v, i, loc) ->
          if IdentSet.mem v used
            then include_stmt stmt
            else M.unit
      | Stmt_Assign(LExpr_Var(v), r, loc) ->
          if IdentSet.mem v used
            then include_stmt stmt
            else M.unit
      | Stmt_If(c, tstmts, elsif, fstmts, loc) ->
          let* (_, tstmts') = M.locally_ (remove_unused' tstmts) in
          let* (_, fstmts') = M.locally_ (remove_unused' tstmts) in
          let* elsif' = M.traverse
            (fun (S_Elsif_Cond (c,ss)) ->
              let+ (_,ss') = M.locally_ (remove_unused' ss) in
              S_Elsif_Cond(c,ss')) elsif in
          (match tstmts',elsif',fstmts' with
          | [], [], [] -> M.unit
          | _ -> include_stmt (Stmt_If(c, tstmts', elsif', fstmts', loc)))
      | _ -> include_stmt stmt
    ) (List.rev xs)

  let go xs =
    let ((), _, xs') = remove_unused' xs () IdentSet.empty in
    List.rev xs'

end

(*
type expr =
    Expr_If of expr * expr * e_elsif list * expr
  | Expr_Binop of expr * binop * expr
  | Expr_Unop of unop * expr
  | Expr_Field of expr * ident
  | Expr_Fields of expr * ident list
  | Expr_Slices of expr * slice list
  | Expr_In of expr * pattern
  | Expr_Var of ident
  | Expr_Parens of expr
  | Expr_Tuple of expr list
  | Expr_Unknown of ty
  | Expr_ImpDef of ty * typeid option
  | Expr_TApply of ident * expr list * expr list
  | Expr_Array of expr * expr
  | Expr_LitInt of typeid
  | Expr_LitHex of typeid
  | Expr_LitReal of typeid
  | Expr_LitBits of typeid
  | Expr_LitMask of typeid
  | Expr_LitString of typeid
*)

(*

We would like to implement a method for recursing and looking for structures
of the form:

Expr_Slices(Expr_TApply("add_int", ...(Expr_TApply("cvt_bits_unit", x)), [0:64])

*)

module Bits = struct

  (** Exception raised by the bitvector conversion visitor when an irreducible
      expression is reached which is not a bitvector type.data

      Examples of cases like this are: an arbitrary variable name, or non-bits literal.
  *)
  exception Non_bits_atomic of expr

  let bits_final width = object
    inherit Asl_visitor.nopAslVisitor

    method! vexpr e = match e with
    | Expr_LitBits (typeid) -> SkipChildren
    | Expr_TApply (FIdent ("cvt_bits_uint", 0), [Expr_LitInt width], [e]) ->
      Printf.printf "base expr: %s\n" (pp_expr e);
      ChangeTo e

    | Expr_TApply (ident, tes, es) -> ChangeDoChildrenPost (e,
      function
      | Expr_TApply (ident, tes, es) -> Printf.printf "f: %s\n" (pprint_ident ident);
          Expr_TApply (Ident (pprint_ident ident ^ "but bits"), tes, es)
      | x -> x
      )
    | Expr_If (c, te, elsif, fe) -> raise (Non_bits_atomic e)
    | Expr_Binop (x, binop, r) -> raise (Non_bits_atomic e)
    | Expr_Unop (unop, expr) -> raise (Non_bits_atomic e)
    | Expr_Field (expr, ident) -> raise (Non_bits_atomic e)
    | Expr_Fields (expr, idents) -> raise (Non_bits_atomic e)
    | Expr_Slices (expr, slices) -> raise (Non_bits_atomic e)
    | Expr_In (expr, pattern) -> raise (Non_bits_atomic e)
    | Expr_Parens (expr) -> raise (Non_bits_atomic e)
    | Expr_Tuple (es) -> raise (Non_bits_atomic e)
    | Expr_Unknown (ty) -> raise (Non_bits_atomic e)
    | Expr_ImpDef (ty, opt) -> raise (Non_bits_atomic e)
    | Expr_Array (expr, i) -> raise (Non_bits_atomic e)

    | Expr_Var (ident) -> raise (Non_bits_atomic e)

    | Expr_LitInt (typeid) -> raise (Non_bits_atomic e)
    | Expr_LitHex (typeid) -> raise (Non_bits_atomic e)
    | Expr_LitReal (typeid) -> raise (Non_bits_atomic e)
    | Expr_LitMask (typeid) -> raise (Non_bits_atomic e)
    | Expr_LitString (typeid) -> raise (Non_bits_atomic e)
  end

  let bits_initial = object
    inherit Asl_visitor.nopAslVisitor

    method! vexpr e = match e with
    | Expr_Slices (expr, [Slice_LoWd(Expr_LitInt "0", Expr_LitInt "64")]) ->
       ChangeTo (let expr' = try Asl_visitor.visit_expr (bits_final "64") expr
        with Non_bits_atomic x -> Printf.printf "non-bits-atomic: %s\n" (pp_expr x); expr
        in Expr_Slices (expr', [Slice_LoWd(Expr_LitInt "0", Expr_LitInt "64")]))
    | _ -> DoChildren
  end


end


let rec rec_expr (step: expr -> expr) (e: expr): expr =
  let go = rec_expr step in
  match e with
  | Expr_If (c, te, elsif, fe) ->
    step (Expr_If (go c, go te, List.map (rec_elsif step) elsif, go fe))
  | Expr_Binop (x, binop, r) ->
    step (Expr_Binop (go x, binop, go r))
  | Expr_Unop (unop, expr) ->
    step (Expr_Unop (unop, go expr))
  | Expr_Field (expr, ident) -> assert false
  | Expr_Fields (expr, idents) -> assert false
  | Expr_Slices (expr, slices) -> assert false
  | Expr_In (expr, pattern) -> assert false
  | Expr_Parens (expr) -> assert false
  | Expr_Tuple (es) -> assert false
  | Expr_Unknown (ty) -> assert false
  | Expr_ImpDef (ty, opt) -> assert false
  | Expr_TApply (ident, tes, es) -> assert false
  | Expr_Array (expr, i) -> assert false

  | Expr_Var (ident) -> e
  | Expr_LitInt (typeid) -> e
  | Expr_LitHex (typeid) -> e
  | Expr_LitReal (typeid) -> e
  | Expr_LitBits (typeid) -> e
  | Expr_LitMask (typeid) -> e
  | Expr_LitString (typeid) -> e

and rec_elsif (f: expr -> expr): e_elsif -> e_elsif =
  function
  | E_Elsif_Cond (c, e) -> E_Elsif_Cond (rec_expr f c, rec_expr f e)
