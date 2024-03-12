open LibASL 
open Asl_ast
open Primops

(* TODO: Normalise these in the IR *)
let mkBits x y = Primops.mkBits (Z.to_int x) y
let from_bitsLit x =
  let x' = Value.drop_chars x ' ' in
  (Primops.mkBits (String.length x') (Z.of_string_base 2 x'))

(* TODO: These should not be necessary *)
let v_FALSE = false
let v_TRUE = true
let f_not_bool = not

let f_mul_int = Z.mul
let f_add_int = Z.add

let f_eq_bits          _ x y = Primops.prim_eq_bits x y
let f_ne_bits          _ x y = Primops.prim_ne_bits x y
let f_add_bits         _ x y = Primops.prim_add_bits x y
let f_sub_bits         _ x y = Primops.prim_sub_bits x y
let f_mul_bits         _ x y = Primops.prim_mul_bits x y
let f_and_bits         _ x y = Primops.prim_and_bits x y
let f_or_bits          _ x y = Primops.prim_or_bits  x y
let f_eor_bits         _ x y = Primops.prim_eor_bits x y
let f_not_bits         _ x   = Primops.prim_not_bits x
let f_slt_bits         _ x y = Primops.prim_lt_int (Primops.prim_cvt_bits_sint x) (Primops.prim_cvt_bits_sint y)
let f_sle_bits         _ x y = Primops.prim_le_int (Primops.prim_cvt_bits_sint x) (Primops.prim_cvt_bits_sint y)
let f_zeros_bits             = Primops.prim_zeros_bits
let f_ones_bits              = Primops.prim_ones_bits
let f_replicate_bits _ _ x y = Primops.prim_replicate_bits x y
let f_append_bits    _ _ x y = Primops.prim_append_bits x y

let f_ZeroExtend     n m (x : bitvector) _ = { v = x.v ; n = Z.to_int m }
let f_SignExtend     n m (x : bitvector) _ = { v = x.v ; n = Z.to_int m }

let f_lsl_bits       w _ (x : bitvector) (y : bitvector) = mkBits w (Z.shift_left x.v (Z.to_int y.v))
let f_lsr_bits       w _ (x : bitvector) (y : bitvector) = mkBits w (Z.shift_right_trunc x.v (Z.to_int y.v))
let f_asr_bits       w _ (x : bitvector) (y : bitvector) = mkBits w (Z.shift_right x.v (Z.to_int y.v))

let extract_bits = Primops.prim_extract

(* IR creation *)

let v_PSTATE_C = Expr_Field(Expr_Var(Ident "PSTATE"), Ident "C")
let v_PSTATE_Z = Expr_Field(Expr_Var(Ident "PSTATE"), Ident "Z")
let v__PC      = Expr_Var(Ident "PC")
let v__R       = Expr_Var(Ident "_R")

let stmts = ref []
let push_stmt s =
  stmts := !stmts @ [s]
let get_res () = !stmts
let clear_res () =
  stmts := []

let loc = Unknown
let expr_of_z i = Expr_LitInt(Z.to_string i)
let type_bits w = Type_Bits(expr_of_z w)
let lt_bt_to_rt_int i = expr_of_z (Primops.prim_cvt_bits_uint i)

let rec to_lexpr = function
  | Expr_Var v -> LExpr_Var v
  | Expr_Field (e,f) -> LExpr_Field (to_lexpr e, f)
  | Expr_Array (e,i) -> LExpr_Array (to_lexpr e, i)
  | _ -> failwith "to_lexpr"

(* Convert a lifttime bitvector into a runtime bitvector *)
let f_gen_bit_lit w (bv: bitvector) = 
  Expr_LitBits (Z.format ("%0" ^ string_of_int bv.n ^ "b") bv.v) 

(* Dynamic variable creation *)
let f_decl_bv name width =
  push_stmt (Asl_ast.Stmt_VarDeclsNoInit(type_bits width, [Ident name], Unknown)) ;
  Expr_Var (Ident name)

(* Variable accesses *)
let f_gen_load v = v
let f_gen_store v e =
  push_stmt (Stmt_Assign(to_lexpr v, e, loc))

(* Array accesses, where the index variable is a liftime bitvector *)
let f_gen_array_load a i =
  Expr_Array(a, lt_bt_to_rt_int i)
let f_gen_array_store a i e =
  push_stmt (Stmt_Assign(LExpr_Array(to_lexpr a, lt_bt_to_rt_int i), e, loc))

(* Prim bit ops *)
let f_gen_eq_bits w e1 e2 =
  Expr_TApply (FIdent ("eq_bits", 0), [expr_of_z w], [e1;e2])
let f_gen_or_bits w e1 e2 =
  Expr_TApply (FIdent ("or_bits", 0), [expr_of_z w], [e1;e2])
let f_gen_and_bits w e1 e2 =
  Expr_TApply (FIdent ("and_bits", 0), [expr_of_z w], [e1;e2])
let f_gen_add_bits w e1 e2 =
  Expr_TApply (FIdent ("add_bits", 0), [expr_of_z w], [e1;e2])
let f_gen_append_bits xw yw x y = 
  Expr_TApply (FIdent ("append_bits", 0), [expr_of_z xw; expr_of_z yw], [x;y])
let f_gen_lsr_bits xw yw x y = 
  Expr_TApply (FIdent ("lsr_bits", 0), [expr_of_z xw; expr_of_z yw], [x;y])
let f_gen_lsl_bits xw yw x y = 
  Expr_TApply (FIdent ("lsl_bits", 0), [expr_of_z xw; expr_of_z yw], [x;y])
let f_gen_replicate_bits xw yw x y = 
  Expr_TApply (FIdent ("replicate_bits", 0), [expr_of_z xw; expr_of_z yw], [x; expr_of_z y])
let f_gen_ZeroExtend xw yw x y =
  Expr_TApply (FIdent ("ZeroExtend", 0), [expr_of_z xw; expr_of_z yw], [x; expr_of_z y])
let f_gen_slice e lo wd = 
  Expr_Slices (e, [Slice_LoWd(expr_of_z lo, expr_of_z wd)])


(*

(* old... *)
let f_gen_branch c =
  (0, 1, 2)

let f_gen_extract_int e lo wd =
  Expr_TApply (FIdent ("slice", 0), [], [e])

let f_gen_cvt_bits_sint w e = e

let f_gen_cvt_bits_uint w e = e

let f_switch_context id =
  ()

let impdef _ = false

let list_update l i e = !l *)
(*let extract_bits_int = Primops.prim_extract_int
let f_extract_int = Primops.prim_extract_int*)
