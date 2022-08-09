module AST = Asl_ast

open AST
open Value
open Asl_utils

type sym_type =
  | T_Bool
  | T_Integer
  | T_Real
  | T_String
  | T_Bits of int
  | T_Tuple of sym_type list

  | T_Enum of ty * ident list
  | T_Array of ixtype * sym_type
  | T_Record of ty * (ty * ident) list
  | T_Register of ty * (slice list * ident) list

let rec pp_sym_type = function
  | T_Bool -> "T_Bool"
  | T_Integer -> "T_Integer"
  | T_Real -> "T_Real"
  | T_String -> "T_String"
  | T_Bits i -> "T_Bits(" ^ string_of_int i ^ ")"
  | T_Tuple ts -> "T_Tuple(" ^ String.concat "," (List.map pp_sym_type ts) ^ ")"
  | T_Enum (ty, _) -> "T_Enum(" ^ pp_type ty ^ ")"
  | T_Array (ix, ty) -> "T_Array(" ^ pp_ixtype ix ^ "," ^ pp_sym_type ty ^ ")"
  | T_Record (ty, _) -> "T_Record(" ^ pp_type ty ^ ")"
  | T_Register (ty, _) -> "T_Register(" ^ pp_type ty ^ ")"

let rec ty_of_sym_type = function
  | T_Bool -> Tcheck.type_bool
  | T_Integer -> Tcheck.type_integer
  | T_Real -> Tcheck.type_real
  | T_String -> Tcheck.type_string
  | T_Bits i -> Tcheck.type_bitsK (string_of_int i)
  | T_Tuple ts -> Type_Tuple (List.map ty_of_sym_type ts)
  | T_Enum (ty, _) -> ty
  | T_Array (ix, ty) -> Type_Array(ix, ty_of_sym_type ty)
  | T_Record (ty, _) -> ty
  | T_Register (ty, _) -> ty


type sym =
  | Val of sym_type * value
  | Exp of sym_type * expr

let is_val (x: AST.expr): bool =
    (match x with
    | Expr_LitInt _ -> true
    | Expr_LitBits _ -> true
    | Expr_LitReal _ -> true
    | Expr_LitString _ -> true
    | Expr_Tuple _ -> true
    | x -> false
    )

let sym_type = function
  | Val (t, _) -> t
  | Exp (t, _) -> t

let sym_type_width x =
  match (x) with
  | T_Bits w -> w
  | t -> failwith ("expected bits type but got: " ^ pp_sym_type t)

let sym_type_append_bits x y =
  let n = sym_type_width x
  and m = sym_type_width y in
  T_Bits (n+m)

let sym_type_field t i =
  match (t) with
  | T_Record (_, fields) -> Tcheck.get_recordfield Unknown fields i
  | _ -> failwith ("expected record type but got: " ^ pp_sym_type t)

let rec val_expr (v: Value.value): AST.expr =
  match v with
  | VBool b -> Expr_Var(if b then Ident "TRUE" else Ident "FALSE")
  | VEnum (id, n) -> Expr_LitInt(string_of_int n)
  | VInt n -> Expr_LitInt(Z.to_string n)
  | VReal n -> Expr_LitReal(Q.to_string n)
  | VBits {n; v} -> Expr_LitBits(Z.to_string v)
  | VString s -> Expr_LitString(s)
  | VTuple vs -> Expr_Tuple(List.map val_expr vs)
  | _ -> failwith @@ "Casting unhandled value type to expression: " ^ pp_value v

let rec val_initialised (v: value): bool =
  match v with
  | VUninitialized _ -> false
  | VRecord bs -> Bindings.for_all (fun _ -> val_initialised) bs
  | VTuple vs -> List.for_all val_initialised vs
  | VArray (vs, _) -> Primops.ImmutableArray.for_all (fun _ -> val_initialised) vs
  | _ -> true

let [@warning "-32"] rec expr_to_lexpr (e: expr): lexpr =
  match e with
  | Expr_Var v -> LExpr_Var v
  | Expr_Tuple es -> LExpr_Tuple (List.map expr_to_lexpr es)
  | _ -> raise (EvalError (Unknown, "unexpected expression in expr_to_lexpr coercion: " ^ pp_expr e))


let filter_uninit (v: value option): value option =
  match v with
  | Some (VUninitialized _) -> None
  | _ -> v

let sym_val_or_uninit (x: sym): value =
  match x with
  | Val (_, v) -> v
  | Exp (_, e) -> VUninitialized (Type_OfExpr e)

let sym_expr (x: sym): expr =
  match x with
    | Val (_, v) -> val_expr v
    | Exp (_, e) -> e

let sym_val (x: sym): value =
  match x with
  | Val (_, v) -> v
  | Exp (_, e) -> failwith ("failed to get value from sym: " ^ pp_expr e)

let sym_pair_has_exp (pair: sym * sym): bool =
  match pair with
  | Exp _, _ -> true
  | _, Exp _ -> true
  | _ -> false

let sym_initialised (x: sym): sym option =
  match x with
  | Val (_, v) -> if val_initialised v then Some x else None
  | Exp _ -> Some x

(** Deconstructs the given list of symbolics.
    Returns a Right of values if the entire list was concrete values,
    otherwise returns a Left of everything coerced to expressions. *)
let rec sym_tuple (xs: sym list): sym =
  match xs with
  | [] -> Val (T_Tuple [], VTuple [])
  | Val (t,v)::xs ->
    (match sym_tuple xs with
    | Val (T_Tuple ts,VTuple vs) -> Val (T_Tuple (t::ts), VTuple (v::vs))
    | Exp (T_Tuple ts,Expr_Tuple es) -> Exp (T_Tuple (t::ts), Expr_Tuple (val_expr v :: es))
    | _ -> failwith "expected tuples from sym_tuple call.")
  | _ -> Exp (T_Tuple (List.map sym_type xs), Expr_Tuple (List.map sym_expr xs))

let pp_sym (rs: sym): string =
    match rs with
    | Val (_, v) -> Printf.sprintf "Val(%s)" (pp_value v)
    | Exp (_, e) -> Printf.sprintf "Exp(%s)" (pp_expr e)

let sym_of_tuple (loc: AST.l) (v: sym): sym list  =
  match v with
  | Val (T_Tuple ts, VTuple vs) -> List.map2 (fun t v -> Val (t,v)) ts vs
  | Exp (T_Tuple ts, Expr_Tuple vs) -> List.map2 (fun t v -> Exp (t,v)) ts vs
  | _ -> raise (EvalError (loc, "tuple value or expression expected. Got "^ pp_sym v))

(* Types *)

(* Primatives *)

(** Apply a primitive operation to two arguments
    TODO: This will fail to evaluate if the primitive requires a type argument.
          Need to derive this from the arguments somehow.
  *)
let prim_binop (f: string) (rty: sym_type) (targs: value list) (loc: AST.l) (x: sym) (y: sym) : sym  =
  (match (x,y) with
  | (Val (_, x),Val (_, y)) ->
      (match eval_prim f targs [x;y] with
      | Some v -> Val (rty, v)
      | None -> raise (EvalError (loc, "Unknown primitive operation: "^ f)))
  | (x,y) -> Exp (rty, Expr_TApply(FIdent(f,0), List.map val_expr targs, (sym_expr x)::[sym_expr y])))

(** Apply a primitive operation to two arguments with return type and type arguments
    possibly dependent on the argument expressions.  *)
let prim_binop_dep (f: string) (make: sym -> sym -> (sym_type * value list)) loc x y =
  let (rty, targs) = make x y in
  prim_binop f rty targs loc x y

let sym_bool_and = prim_binop "bool_and" T_Bool []

let sym_true     = Val (T_Bool, from_bool true)
let sym_false    = Val (T_Bool, from_bool false)
let sym_eq_int   = prim_binop "eq_int" T_Integer []
let sym_leq      = prim_binop "leq" T_Integer []
let sym_add_int  = prim_binop "add_int" T_Integer []
let sym_sub_int  = prim_binop "sub_int" T_Integer []


let sym_eq_bool  = prim_binop "eq_enum" T_Bool []
let sym_eq_enum  = prim_binop "eq_enum" T_Bool []
let sym_eq_real  = prim_binop "eq_real" T_Bool []
let sym_eq_string = prim_binop "eq_str" T_Bool []
let sym_eq_bits  = prim_binop_dep "eq_bits"
  (fun x y -> (T_Integer, [VInt (Z.of_int (sym_type_width (sym_type x)))]))

let sym_inmask   = prim_binop_dep "in_mask"
  (fun x y -> (T_Integer, [VInt (Z.of_int (sym_type_width (sym_type x)))]))

let sym_append_bits (x: sym) (y: sym) =
  let t = sym_type_append_bits (sym_type x) (sym_type y) in
  (match (x,y) with
  | (Val (_, VBits x),Val (_, VBits y)) -> Val (t, VBits (Primops.prim_append_bits x y))
  | (Val _, Val _) -> failwith ("expected bits values in sym_append_bits but got: " ^ pp_sym x ^ ", " ^ pp_sym y)
  | (x,y) -> Exp (t, Expr_TApply(FIdent("append_bits",0), [], (sym_expr x)::[sym_expr y])))


let rec sym_slice (loc: l) (x: sym) (lo: int) (wd: int): sym =
  let int_expr i = Expr_LitInt (string_of_int i) in
  let ty = T_Bits wd in
  match x with
  | Val (_, v) -> Val (ty, extract_bits' loc v lo wd)
  | Exp (_, e) ->
    let slice_expr =
      (Expr_Slices (e, [Slice_LoWd (int_expr lo, int_expr wd)])) in
    (match e with
    | (Expr_TApply (FIdent ("append_bits", 0), [Expr_LitInt t1; Expr_LitInt t2], [x1; x2])) ->
      let t1 = int_of_string t1 in
      let t2 = int_of_string t2 in
      if (lo >= t2) then
        (* slice is entirely within upper part (i.e. significant bits). *)
        sym_slice loc (Exp (T_Bits t1, x1)) (lo - t2) wd
      else if (lo + wd <= t2) then
        (* entirely within lower part. *)
        sym_slice loc (Exp (T_Bits t2, x2)) lo wd
      else
        Exp (ty, slice_expr)
    | _ -> Exp (ty, slice_expr))

let rec contains_uninit (v: value): bool =
  match v with
  | VUninitialized _ -> true
  | VTuple vs -> List.exists contains_uninit vs
  | VRecord r -> Bindings.exists (fun _ -> contains_uninit) r
  | _ -> false

let sym_prim_simplify (name: string) (tes: sym list) (es: sym list): sym option =

  let vint_eq cmp = function
    | VInt x when Z.equal cmp x -> true
    | _ -> false in

  let [@warning "-26"] is_zero = vint_eq Z.zero
  and [@warning "-26"] is_one = vint_eq Z.one in

  let is_zero_bits = function
    | (VBits {n = _; v = v}) -> Z.equal Z.zero v
    | _ -> false in

  (match (name, tes, es) with
  | ("add_int",     _,                [Val (_, x1); x2])       when is_zero x1 -> Some x2
  | ("add_int",     _,                [x1; Val (_, x2)])       when is_zero x2 -> Some x1
  | ("append_bits", [Val (_, t1); _], [_; x2])                 when is_zero t1 -> Some x2
  | ("append_bits", [_; Val (_, t2)], [x1; _])                 when is_zero t2 -> Some x1
  | ("or_bits",     _,                [Val (_, x1); x2])       when is_zero_bits x1 -> Some x2
  | ("or_bits",     _,                [x1; Val (_, x2)])       when is_zero_bits x2 -> Some x1
  | _ -> None)

let rec val_type (v: value): ty =
  let unsupported () = failwith @@ "val_type unsupported: " ^ pp_value v in
  match v with
  | VBool _ -> type_builtin "boolean"
  | VEnum (ident, _) -> Type_Constructor ident
  | VInt _ -> type_builtin "integer"
  | VReal _ -> type_builtin "real"
  | VBits {n=n; _} -> type_bits (string_of_int n)
  | VString _ -> type_builtin "string"
  | VExc _ -> type_builtin "__Exception"
  | VTuple vs -> Type_Tuple (List.map val_type vs)
  | VRAM _ -> type_builtin "__RAM"
  | VMask mask -> unsupported ()
  | VRecord (_) -> unsupported ()
  | VArray (arr, def) -> unsupported ()
  | VUninitialized ty -> ty

let sym_type =
  function
  | Val (t, v) -> t
  | Exp (t, e) -> t


let stmt_loc (s: stmt): l =
  match s with
  | Stmt_VarDeclsNoInit (_, _, l) -> l
  | Stmt_VarDecl (_, _, _, l) -> l
  | Stmt_ConstDecl (_, _, _, l) -> l
  | Stmt_Assign (_, _, l) -> l
  | Stmt_FunReturn (_, l) -> l
  | Stmt_ProcReturn (l) -> l
  | Stmt_Assert (_, l) -> l
  | Stmt_Unpred (l) -> l
  | Stmt_ConstrainedUnpred (l) -> l
  | Stmt_ImpDef (_, l) -> l
  | Stmt_Undefined (l) -> l
  | Stmt_ExceptionTaken (l) -> l
  | Stmt_Dep_Unpred (l) -> l
  | Stmt_Dep_ImpDef (_, l) -> l
  | Stmt_Dep_Undefined (l) -> l
  | Stmt_See (_, l) -> l
  | Stmt_Throw (_, l) -> l
  | Stmt_DecodeExecute (_, _, l) -> l
  | Stmt_TCall (_, _, _, l) -> l
  | Stmt_If (_, _, _, _, l) -> l
  | Stmt_Case (_, _, _, l) -> l
  | Stmt_For (_, _, _, _, _, l) -> l
  | Stmt_While (_, _, l) -> l
  | Stmt_Repeat (_, _, l) -> l
  | Stmt_Try (_, _, _, _, l) -> l
