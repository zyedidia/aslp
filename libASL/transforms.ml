open Asl_utils

open AST
open Visitor

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
      | (FIdent ("sub_int", 0), []) -> (FIdent ("sub_bits", 0), [Expr_LitInt width])
      | (FIdent ("mul_int", 0), []) -> (FIdent ("mul_bits", 0), [Expr_LitInt width])
      | _ -> (f, tes)

    method! vexpr e = match e with
    | Expr_LitBits (s) when String.length s = int_of_string width -> SkipChildren

    | Expr_TApply (FIdent ("cvt_int_real", _), _, _) -> raise (Non_bits_atomic e)

    | Expr_TApply (FIdent ("cvt_bits_uint", 0), [Expr_LitInt w], [e]) ->
      let w = int_of_string w in
      let width = int_of_string width in
      let padwidth = width - w in
      if padwidth = 0 then
        (* width matches what we want. just return original bitvector expression.  *)
        ChangeTo e
      else if padwidth > 0 then
        (* expression width is shorter than what we want, pad it with padwidth zeros. *)
        let pad = String.init padwidth (fun _ -> '0') in
        ChangeTo (
          Expr_TApply (FIdent ("append_bits", 0),
          [Expr_LitInt (string_of_int padwidth); Expr_LitInt (string_of_int w)],
          [Expr_LitBits pad; e])
        )
      else
        raise (Non_bits_atomic e)

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
        fun e ->
          match e with
          | Expr_Slices (expr, [Slice_LoWd(Expr_LitInt "0", Expr_LitInt w)]) when w = width ->
            (try Asl_visitor.visit_expr coerce_visitor expr
            with | Non_bits_atomic _ -> e)
          | _ -> e)
  end

  (** Performs the bitvector expression coercion on the given statement list.  *)
  let bitvec_conversion (xs: stmt list): stmt list =
    Asl_visitor.visit_stmts (bits_traverse_begin "64") xs
end


module RefParams = struct

  let has_ref_param (xs: sformal list) =
    List.exists
      (function
      | Formal_InOut _ -> true
      | _ -> false)
      xs

  (** Filters the given list of sformal, returning a list of
      (argument index, type, argument name) with only the ref params. *)
  let get_ref_params (xs: sformal list): (int * ty * ident) list =
    let xs = List.mapi (fun i x -> (i,x)) xs in
    List.filter_map
      (fun (n,f) ->
      match f with
      | Formal_InOut (t,i) -> Some (n,t,i)
      | _ -> None)
      xs

  let replace_returns ss s =
    let visit = object
      inherit Asl_visitor.nopAslVisitor
      method! vstmt =
        function
        | Stmt_ProcReturn _ -> ChangeTo s
        | Stmt_FunReturn _ -> failwith "unexpected function return in ref param conversion."
        | _ -> DoChildren
    end
    in
    Asl_visitor.visit_stmts visit ss

  class visit_decls = object
    inherit Asl_visitor.nopAslVisitor

    (* mapping of function identifiers to the indices of their
       reference parameters. *)
    val mutable ref_params : int list Bindings.t = Bindings.empty

    method ref_params = ref_params

    method! vdecl (d: declaration): declaration visitAction =
      match d with
      | Decl_ArraySetterDefn (nm, args, vty, vnm, body, loc)->
        (match get_ref_params args with
        | [] -> DoChildren
        | refs ->
          let ns = List.map (fun (n,_,_) -> n) refs in
          let ts = List.map (fun (_,t,_) -> t) refs in
          let is = List.map (fun (_,_,i) -> i) refs in

          ref_params <- Bindings.add nm ns ref_params;

          (* append setter value argument to formal argument list. *)
          let args' = List.map Tcheck.formal_of_sformal args @ [vty, vnm] in

          (* construct return expression to return modified ref vars. *)
          let vars = List.map (fun x -> Expr_Var x) is in
          let ret = Stmt_FunReturn (Expr_Tuple vars, loc) in
          let body' = replace_returns body ret in

          let rty = Type_Tuple ts in
          ChangeTo (Decl_FunDefn (rty, nm, args', body', loc))
        )
      | _ -> DoChildren
  end

  class visit_writes ref_params = object
    inherit Asl_visitor.nopAslVisitor

    method! vstmt (s: stmt): stmt visitAction =
      match s with
      | Stmt_Assign (LExpr_Write (setter, targs, args), r, loc) ->
        (* Printf.printf " %s\n" (pp_stmt s); *)
        (match Bindings.find_opt setter ref_params with
        | None -> DoChildren
        | Some ns ->
          let refs = List.map (List.nth args) ns in
          (* Printf.printf "ref param: %s\n" (pp_expr a); *)

          let les = List.map Symbolic.expr_to_lexpr refs in
          let call = Expr_TApply (setter, targs, args @ [r]) in
          ChangeTo (Stmt_Assign (LExpr_Tuple les, call, loc))
        )
      | _ -> DoChildren
  end

  let ref_param_conversion (ds: declaration list) =
    let v1 = new visit_decls in
    let ds = List.map (Asl_visitor.visit_decl (v1 :> Asl_visitor.aslVisitor)) ds in
    let v2 = new visit_writes (v1#ref_params) in
    let ds = List.map (Asl_visitor.visit_decl v2) ds in
    ds
end
