open Asl_utils

open AST
open Visitor
open Symbolic
open Value

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


module Bits2 = struct
  type interval = (Z.t * Z.t)
  let empty_interval = (Z.zero, Z.minus_one)

  let type_bits n = Type_Bits (Expr_LitInt (string_of_int n))
  let type_bits_var i = Type_Bits (Expr_Var i)

  let num_bits_unsigned n =
    assert (Z.geq n Z.zero);
    if Z.equal n Z.zero then
      0
    else
      Z.log2 n + 1

  let num_bits_signed n =
    if Z.geq n Z.zero then
      num_bits_unsigned n + 1
    else
      num_bits_unsigned (Z.sub (Z.abs n) Z.one) + 1

  let size_of_interval (lo,hi) =
    assert (Z.leq lo hi);
    max (max (num_bits_signed lo) (num_bits_signed hi)) 1

  let type_interval x =
    type_bits (size_of_interval x)

  let is_empty (x,y) = Z.equal x Z.zero && Z.equal y Z.minus_one

  let interval_join (x: interval) (y: interval) =
    match x,y with
    | _,_ when is_empty x || is_empty y -> assert false
    | (x1,x2),(y1,y2) -> Z.min x1 y1, Z.max x2 y2

  let interval_fold (xs: interval list) =
    match xs with
    | [] -> assert false
    | x::xs -> List.fold_left interval_join x xs

  let interval_of_size (n: int): interval =
    assert (n >= 1);
    let magnitude = Z.shift_left Z.one (n - 1) in
    (Z.neg magnitude, Z.sub magnitude Z.one)



  (* interpret a bit literal as a SIGNED integer. *)
  let sint_of_bits x =
    let x = Value.drop_chars x ' ' in
    let len = String.length x in
    Z.signed_extract (Z.of_string_base 2 x) 0 len


  (* returns the interval of an expression. expression must evaluate to an integer. *)
  let rec interval_of_expr (vars: interval Bindings.t) (e: expr): interval =
    match e with
    | Expr_Binop _ -> assert false
    | Expr_Unop _ -> assert false
    | Expr_Field (expr, ident) -> assert false
    | Expr_Fields (expr, ident_list) -> assert false
    | Expr_Slices (expr, slice_list) ->
      let wds = List.map (function | Slice_LoWd (_,Expr_LitInt n) -> int_of_string n | _ -> assert false) slice_list in
      let wd = List.fold_left (+) 0 wds in
      interval_of_size wd
    | Expr_In (expr, pattern) -> assert false
    | Expr_Var (ident) -> (Bindings.find ident vars)
    | Expr_Parens (expr) -> interval_of_expr vars expr
    | Expr_Tuple (expr_list) -> assert false
    | Expr_Unknown (ty) -> assert false
    | Expr_ImpDef (ty, str_option) -> assert false
    | Expr_TApply (ident, expr_list, expr_list2) -> assert false (* TODO *)
    | Expr_If (ty, cond, t, e_elsif_list, f) -> assert false
    | Expr_Array (expr, expr2) -> assert false
    | Expr_LitInt (s)
    | Expr_LitHex (s) -> let n = Z.of_string s in (n,n)
    | Expr_LitReal (str) -> assert false
    | Expr_LitBits (str) -> let n = sint_of_bits str in (n,n)
    | Expr_LitMask (str) -> assert false
    | Expr_LitString (str) -> assert false

  let bits_size_of_expr (vars: interval Bindings.t) (e: expr): int =
    match e with
    | Expr_TApply (fn, tes, es) ->
      (match (fn, tes, es) with
      | FIdent ("add_bits", 0), [Expr_LitInt n], _
      | FIdent ("sub_bits", 0), [Expr_LitInt n], _
      | FIdent ("mul_bits", 0), [Expr_LitInt n], _
      | FIdent ("and_bits", 0), [Expr_LitInt n], _
      | FIdent ("or_bits", 0), [Expr_LitInt n], _
      | FIdent ("eor_bits", 0), [Expr_LitInt n], _
      | FIdent ("not_bits", 0), [Expr_LitInt n], _
      | FIdent ("zeros_bits", 0), [Expr_LitInt n], _
      | FIdent ("ones_bits", 0), [Expr_LitInt n], _ -> int_of_string n
      | FIdent ("append_bits", 0), [Expr_LitInt n; Expr_LitInt m], _ -> int_of_string n + int_of_string m
      | _ -> failwith @@ "bits_size_of_expr: unhandled " ^ pp_expr e
      )
    | Expr_Slices (_, slice_list) ->
      let wds = List.map (function | Slice_LoWd (_,Expr_LitInt n) -> int_of_string n | _ -> assert false) slice_list in
      let wd = List.fold_left (+) 0 wds in
      wd
    | _ -> size_of_interval (interval_of_expr vars e)

  let bits_size_of_val (v: value): int =
    match v with
    | VBits {n=n; _} -> n
    | _ -> failwith @@ "bits_size_of_val: unhandled " ^ pp_value v

  let bits_size_of_sym (vars: interval Bindings.t) = function
    | Val v -> bits_size_of_val v
    | Exp e -> bits_size_of_expr vars e

  let expr_sign_extend (vars: interval Bindings.t) (size: int) (e: sym) =
    let old = bits_size_of_sym vars e in
    assert (old <= size);
    if old = size then
      e
    else
      (sym_sign_extend (size - old) old e)

  class bits_traverse_coerce = object (self)
    inherit Asl_visitor.nopAslVisitor

    val mutable count = 0;

    val mutable vars : ident Bindings.t = Bindings.empty
    val mutable intervals : interval Bindings.t = Bindings.empty


    method make_var nm =
      count <- count + 1;
      let var = Ident (pprint_ident nm ^ "_size__" ^ string_of_int count) in
      vars <- Bindings.add nm var vars;
      type_bits_var var

    method! vstmt s =
      ChangeDoChildrenPost (s, fun s ->
        match s with
        | Stmt_VarDecl (Type_Constructor (Ident "integer"), nm, r, l) ->
          let interval = interval_of_expr intervals r in
          intervals <- Bindings.add nm interval intervals;
          let t = self#make_var nm in
          Stmt_VarDecl (t, nm, r, l)
        | _ -> s
      )

    method! vexpr e =
      match e with
      | Expr_LitInt n ->
        let n' = Z.of_string n in
        let size = num_bits_signed n' in
        let a = Z.extract n' 0 size in
        ChangeTo (Expr_LitBits (Z.format ("%0" ^ string_of_int size ^ "b") a))
      | Expr_TApply (fn, tes, es) ->
        (* Printf.printf "going down\n"; *)
        ChangeDoChildrenPost (e, fun e' ->
            (* Printf.printf "going up with %s\n" (pp_expr e); *)
          match e' with

          | Expr_TApply (FIdent ("cvt_bits_uint", 0), [_], [e]) ->
            sym_expr @@ sym_zero_extend 1 (int_of_expr (List.hd tes)) (sym_of_expr e)
          | Expr_TApply (FIdent ("cvt_bits_sint", 0), [_], [e]) ->
            e
          | Expr_TApply (FIdent ("add_int", 0), [], [x;y]) ->
            let xsize = bits_size_of_expr intervals x in
            let ysize = bits_size_of_expr intervals y in
            let size = max xsize ysize + 1 in
            let ex e = expr_sign_extend intervals size (sym_of_expr e) in
            (* Printf.printf "x %s\ny %s\n" (pp_expr x) (pp_expr y) ; *)
            sym_expr @@ sym_prim (FIdent ("add_bits", 0)) [sym_int size] [ex x; ex y]

          | Expr_TApply (FIdent ("eq_int", 0), [], [x;y]) ->
            let xsize = bits_size_of_expr intervals x in
            let ysize = bits_size_of_expr intervals y in
            let size = max xsize ysize in
            let ex e = expr_sign_extend intervals size (sym_of_expr e) in
            sym_expr @@ sym_prim (FIdent ("eq_bits", 0)) [sym_int size] [ex x; ex y]

          | Expr_TApply (FIdent ("ne_int", 0), [], [x;y]) ->
            let xsize = bits_size_of_expr intervals x in
            let ysize = bits_size_of_expr intervals y in
            let size = max xsize ysize in
            let ex e = expr_sign_extend intervals size (sym_of_expr e) in
            sym_expr @@ sym_prim (FIdent ("ne_bits", 0)) [sym_int size] [ex x; ex y]

          | Expr_TApply (FIdent (f, 0), _, _) when Utils.endswith f "_int" ->
            failwith @@ "unsupported integer function: " ^ pp_expr e

          | Expr_TApply (fn', _, es') when fn' = fn ->
            Expr_TApply (fn', tes, es')
          | _ -> e'
        )
    | Expr_Slices (base, slices) ->
      ChangeDoChildrenPost (e, function
        | Expr_Slices (base', _) -> Expr_Slices (base', slices)
        | _ -> assert false)
    | Expr_Array (base, ind) ->
      ChangeDoChildrenPost (e, function
        | Expr_Array (base', _) -> Expr_Array (base', ind)
        | _ -> assert false)
    | _ -> DoChildren

    method! vtype x = SkipChildren
    method! vlexpr x = SkipChildren

  end

end
