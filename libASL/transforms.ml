open Asl_utils

open AST
open Visitor
open Symbolic
open Value

(** Remove variables which are unused at the end of the statement list. *)
module RemoveUnused = struct
  let rec remove_unused (globals: IdentSet.t) xs = (remove_unused' globals xs)

  and remove_unused' (used: IdentSet.t) (xs: stmt list): (stmt list) =
    fst @@ List.fold_right (fun stmt (acc, used) ->

      let pass = (acc, used)
      and emit (s: stmt) = (s::acc, IdentSet.union used (fv_stmt s))
      in

      match stmt with
      | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        let vs' = List.filter (fun i -> IdentSet.mem i used) vs in
        (match vs' with
        | [] -> pass
        | _ -> emit (Stmt_VarDeclsNoInit(ty, vs', loc)))
      | Stmt_VarDecl(ty, v, i, loc) ->
        if IdentSet.mem v used
          then emit stmt
          else pass
      | Stmt_ConstDecl(ty, v, i, loc) ->
        if IdentSet.mem v used
          then emit stmt
          else pass
      | Stmt_Assign(le, r, loc) ->
        let lvs = assigned_vars_of_stmts [stmt] in
        if not (IdentSet.disjoint lvs used)
          then emit stmt
          else pass
      | Stmt_If(c, tstmts, elsif, fstmts, loc) ->
        let tstmts' = remove_unused' used tstmts in
        let fstmts' = remove_unused' used fstmts in
        let elsif' = List.map
          (fun (S_Elsif_Cond (c,ss)) ->
            S_Elsif_Cond (c, remove_unused' used ss))
          elsif in
        (match (tstmts',fstmts',elsif') with
        | [], [], [] -> pass
        | _, _, _ -> emit (Stmt_If(c, tstmts', elsif', fstmts', loc)))
      | x -> emit x

    ) xs ([], used)
end



(** Transforms setters using formal reference (in/out) parameters
    into functions returning modified versions of the reference parameters.
*)
module RefParams = struct

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

  (** Replaces all procedure returns in the given statement list
      with the given statement. *)
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

  (** Replaces setter declarations which use formal in-out parameters with
      functions which return their modified parameters.

      For example,

        Elem[bits(N) &vector, integer e] = bits(size) value
          ...
          return;

      is transformed to

        (bits(N)) Elem.read(bits(N) vector, integer e, bits(size) value)
          ...
          return (vector);


      *)
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
          (* indices, types, and identifiers for the ref params. *)
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

  (** Replaces writes to the setters modified above to assign
      the return value back to the original variables.

      For example,

        Elem[vector, 2] = '1001';

      is transformed to

        vector = Elem.read(vector, 2, '1001');

      *)
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



(** Transforms expressions of integers into equivalent expressions over
    bit-vectors. *)
module IntToBits = struct
  type interval = (Z.t * Z.t)
  let empty_interval = (Z.zero, Z.minus_one)


  (** Returns the number of bits needed to represent n (where n >= 0),
      assuming the bits are interpreted as unsigned. *)
  let num_bits_unsigned n =
    assert (Z.geq n Z.zero);
    if Z.equal n Z.zero then
      0
    else
      Z.log2 n + 1

  (** Returns the number of bits needed to represent n, assuming
      the bits are interpreted as signed two's complement.  *)
  let num_bits_signed n =
    if Z.geq n Z.zero then
      (* non-negative n case is same as unsigned + 1 for sign bit. *)
      num_bits_unsigned n + 1
    else
      (* representing -1 requires representing |n|-1 in
         unsigned, then + 1 for sign bit. *)
      num_bits_unsigned (Z.sub (Z.abs n) Z.one) + 1

  (** Returns the number of (signed) bits needed to represent
      all numbers within (lo,hi) inclusive.  *)
  let size_of_interval (lo,hi) =
    assert (Z.leq lo hi);
    max (max (num_bits_signed lo) (num_bits_signed hi)) 1

  (** Returns the interval which is representable by the given number
      of two's complement bits.  *)
  let interval_of_size (n: int): interval =
    assert (n >= 1);
    let magnitude = Z.shift_left Z.one (n - 1) in
    (Z.neg magnitude, Z.sub magnitude Z.one)

  (** Removes all space characters from the given string. *)
  let drop_space x = Value.drop_chars x ' '

  (** Interprets a bit literal as a signed integer. *)
  let sint_of_bits x =
    let x = Value.drop_chars x ' ' in
    let len = String.length x in
    Z.signed_extract (Z.of_string_base 2 x) 0 len


  (** Returns the bit-width of the given expression.
      Requires expression to evaluate to a bit-vector type. *)
  let rec bits_size_of_expr (e: expr): int =
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
      | FIdent ("ZeroExtend", 0), [_; Expr_LitInt m], _
      | FIdent ("SignExtend", 0), [_; Expr_LitInt m], _ -> int_of_string m
      | _ -> failwith @@ "bits_size_of_expr: unhandled " ^ pp_expr e
      )
    | Expr_Parens e -> bits_size_of_expr e
    | Expr_LitBits s -> String.length (drop_space s)
    | Expr_Slices (expr, slice_list) ->
      let wds = List.map (function | Slice_LoWd (_,Expr_LitInt n) -> int_of_string n | _ -> assert false) slice_list in
      List.fold_left (+) 0 wds
    | _ -> assert false

  (** Returns the bit-width of the given value,
      and errors if the value is not a bit value. *)
  let bits_size_of_val (v: value): int =
    match v with
    | VBits {n=n; _} -> n
    | _ -> failwith @@ "bits_size_of_val: unhandled " ^ pp_value v

  (** Returns the bit-width of the given symbolic. *)
  let bits_size_of_sym = function
    | Val v -> bits_size_of_val v
    | Exp e -> bits_size_of_expr e

  (** Extends the given symbolic to the given size,
      treating it as a signed two's complement expression. *)
  let bits_sign_extend (size: int) (e: sym) =
    let old = bits_size_of_sym e in
    assert (old <= size);
    if old = size
      then e
      else (sym_sign_extend (size - old) old e)

  (** Returns a symbolic bits expression of the given expression,
      including coercing integers to two's complement bits where
      needed. *)
  let bits_sym_of_expr e =
    let e' =
      match e with
      | Expr_LitInt n
      | Expr_LitHex n ->
        let n' = Z.of_string (drop_space n) in
        let size = num_bits_signed n' in
        let a = Z.extract n' 0 size in
        (Expr_LitBits (Z.format ("%0" ^ string_of_int size ^ "b") a))
      | _ -> e
    in
    sym_of_expr e'

  (** Transform integer expressions into bit-vector expressions while
      maintaining precision by widening bit-vector sizes as operations
      are applied. *)
  class bits_coerce_widening = object (self)
    inherit Asl_visitor.nopAslVisitor

    method extend size e =
      bits_sign_extend size (bits_sym_of_expr e)


    (** Visits an expression, coercing integer expressions into bit-vector
        operations.

        Bit-vectors generated by this conversion are in SIGNED two's complement.
        Each visit case assumes its sub-expressions have already been converted
        to signed bit-vectors.
    *)
    method! vexpr e =
      match e with

      | Expr_TApply (fn, tes, es) ->
        ChangeDoChildrenPost (e, fun e' ->
          match e' with

          | Expr_TApply (FIdent ("cvt_bits_uint", 0), [t], [e]) ->
            sym_expr @@ sym_zero_extend 1 (int_of_expr t) (bits_sym_of_expr e)
          | Expr_TApply (FIdent ("cvt_bits_sint", 0), [_], [e]) ->
            e
          | Expr_TApply (FIdent ("add_int", 0), [], [x;y]) ->
            let xsize = bits_size_of_expr x in
            let ysize = bits_size_of_expr y in
            let size = max xsize ysize + 1 in
            let ex = self#extend size in
            (* Printf.printf "x %s\ny %s\n" (pp_expr x) (pp_expr y) ; *)
            sym_expr @@ sym_prim (FIdent ("add_bits", 0)) [sym_of_int size] [ex x; ex y]

          | Expr_TApply (FIdent ("sub_int", 0), [], [x;y]) ->
            let xsize = bits_size_of_expr x in
            let ysize = bits_size_of_expr y in
            let size = max xsize ysize + 1 in
            let ex = self#extend size in
            (* Printf.printf "x %s\ny %s\n" (pp_expr x) (pp_expr y) ; *)
            sym_expr @@ sym_prim (FIdent ("sub_bits", 0)) [sym_of_int size] [ex x; ex y]

          | Expr_TApply (FIdent ("eq_int", 0), [], [x;y]) ->
            let xsize = bits_size_of_expr x in
            let ysize = bits_size_of_expr y in
            let size = max xsize ysize in
            let ex = self#extend size in
            sym_expr @@ sym_prim (FIdent ("eq_bits", 0)) [sym_of_int size] [ex x; ex y]

          | Expr_TApply (FIdent ("ne_int", 0), [], [x;y]) ->
            let xsize = bits_size_of_expr x in
            let ysize = bits_size_of_expr y in
            let size = max xsize ysize in
            let ex = self#extend size in
            sym_expr @@ sym_prim (FIdent ("ne_bits", 0)) [sym_of_int size] [ex x; ex y]

          | Expr_TApply (FIdent ("mul_int", 0), [], [x;y]) ->
            let xsize = bits_size_of_expr x in
            let ysize = bits_size_of_expr y in
            let size = xsize + ysize in
            let ex = self#extend size in
            sym_expr @@ sym_prim (FIdent ("mul_bits", 0)) [sym_of_int size] [ex x; ex y]

            (* x >= y  iff  y <= x  iff  x - y >= 0*)
          | Expr_TApply (FIdent ("ge_int", 0), [], [x;y])
          | Expr_TApply (FIdent ("le_int", 0), [], [y;x]) ->
            let xsize = bits_size_of_expr x in
            let ysize = bits_size_of_expr y in
            let size = max xsize ysize in
            let ex x = sym_expr (self#extend size x) in
            expr_prim' "sle_bits" [expr_of_int size] [ex y;ex x]

            (* x < y  iff  y > x  iff x - y < 0 *)
          | Expr_TApply (FIdent ("lt_int", 0), [], [x;y])
          | Expr_TApply (FIdent ("gt_int", 0), [], [y;x]) ->
            let xsize = bits_size_of_expr x in
            let ysize = bits_size_of_expr y in
            let size = max xsize ysize in
            let ex x = sym_expr (self#extend size x) in
            expr_prim' "slt_bits" [expr_of_int size] [ex x;ex y]
          (* NOTE: sle_bits and slt_bits are signed less or equal,
             and signed less than.
             These are not primitive in ASL but are defined in BIL so
             we take advantage of them. *)

          | Expr_TApply (FIdent (f, 0), _, _) when Utils.endswith f "_int" ->
            failwith @@ "unsupported integer function: " ^ pp_expr e'

          | _ -> e'
        )
    | _ -> DoChildren

  end

  (** A second transform pass which narrows bit-vector expressions which are
      later sliced, by considering the bits needed in that final slice. *)
  class bits_coerce_narrow = object (self)
    inherit Asl_visitor.nopAslVisitor

    method! vexpr e =
      match e with
      | Expr_Slices(
          Expr_TApply (FIdent (fn, 0), [_], es),
          [Slice_LoWd (Expr_LitInt lo, Expr_LitInt wd) as sl] ) ->

        let wd' = int_of_string lo + int_of_string wd in
        let slice e = sym_expr @@ sym_slice Unknown (sym_of_expr e) 0 wd' in
        let narrow = Expr_TApply (FIdent (fn, 0), [expr_of_int wd'], List.map slice es) in

        (* for add and sub expressions, we only need the lowest n bits in order
           to have n bits of precision in the output. *)
        (match fn with
        | "add_bits" -> ChangeDoChildrenPost (narrow, fun x -> Expr_Slices (x, [sl]))
        | "sub_bits" -> ChangeDoChildrenPost (narrow, fun x -> Expr_Slices (x, [sl]))
        | _ -> DoChildren
        )
      | _ -> DoChildren

  end

  let ints_to_bits xs =
    xs
    |> Asl_visitor.visit_stmts (new bits_coerce_widening)
    |> Asl_visitor.visit_stmts (new bits_coerce_narrow)

end
