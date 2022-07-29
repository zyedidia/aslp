open Asl_utils

open AST

(* module unused. *)
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


(**

We would like to implement a method for recursing and looking for structures
of the form:

add_int.0 {{  }} (
  cvt_bits_uint.0 {{ 64 }} ( Exp2__2 [ 0 +: 64 ] ),
  cvt_bits_uint.0 {{ 64 }} ( Exp5__3 [ 0 +: 64 ] )
) [ 0 +: 64 ]

and replacing it with an equivalent "pure bitvector expression" like:

add_bits.0 {{ 64 }} (
  Exp2__2 [ 0 +: 64 ],
  Exp5__3 [ 0 +: 64 ]
)


This is done using two visitors over the expression tree.
- First, we search for a bitvector coercion to bits of the form "x[0:+64]"
  using the bits_traverse_entry visitor.
- Then, we visit the subexpression "x" and attempt to convert it into a pure
  bitvector expression using the bits_traverse_to_cvt visitor. If this is
  possible, we replace "x[0:+64]" with this pure bits expression "x'".

*)
module Bits = struct

  (** Exception raised by the bitvector conversion visitor when an irreducible
      expression is reached which is not a bitvector type.data

      Examples of these cases are: an arbitrary variable name, or non-bits literal.
  *)
  exception Non_bits_atomic of expr

  (** Visit the subtree until we reach a pure bitvector expression or a coercion from
      pure bitvector to integer.

      The vexpr method returns the expression as a pure bitvector expression of the
      correct width if that is possible. Otherwise, it throws an exception.
  *)
  class bits_traverse_coerce width = object (self)
    inherit Asl_visitor.nopAslVisitor

    (** Substitutes integer function names and type arguments with their bitvector versions.

        No conversion of argument expressions is done in this function. We assume that
        is done by the vexpr method.
    *)
    method to_bits_fun f tes =
      match (f, tes) with
      | (FIdent ("add_int", 0), []) -> (FIdent ("add_bits", 0), [Expr_LitInt width])
      | _ -> (f, tes)

    method! vexpr e = match e with
    | Expr_LitBits (s) when String.length s = int_of_string width -> SkipChildren

    | Expr_TApply (FIdent ("cvt_bits_uint", 0), [Expr_LitInt w], [e]) when w = width ->
      (* Printf.printf "base expr: %s\n" (pp_expr e); *)
      ChangeTo e

    | Expr_TApply (ident, tes, es) -> ChangeDoChildrenPost (e,
      function
      | Expr_TApply (ident, tes, es) ->
          let (f', tes') = self#to_bits_fun ident tes in
          (* Printf.printf "f: %s\n" (pprint_ident ident); *)
          Expr_TApply (f', tes', es)
      | x -> x
      )

    | _ -> raise (Non_bits_atomic e)
  end

  (** Starts the bitvector conversion by looking for coercions _to_ a bitvector.

      Then calls the bits_traverse_coerce visitor to coerce subexpressions of
      this into pure bitvector expressions if possible.
  *)
  let bits_traverse_begin width = object
    inherit Asl_visitor.nopAslVisitor

    val coerce_visitor = (new bits_traverse_coerce width :> Asl_visitor.aslVisitor)

    method! vexpr e =
      (* post-order visit of x[0:+64] expressions. *)
      ChangeDoChildrenPost (e,
        function
        | Expr_Slices (expr, [Slice_LoWd(Expr_LitInt "0", Expr_LitInt w)]) when w = width ->
          let expr' =
            try Asl_visitor.visit_expr coerce_visitor expr
            with Non_bits_atomic x ->
              (* Printf.printf "non-bits-atomic: %s\n" (pp_expr x); *)
              expr
          in expr'
        | e -> e)
  end

  (** Performs the bitvector expression coercion on the given statement list.  *)
  let bitvec_conversion (xs: stmt list): stmt list =
    Asl_visitor.visit_stmts (bits_traverse_begin "64") xs
end


