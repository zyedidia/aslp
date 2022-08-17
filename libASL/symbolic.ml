module AST = Asl_ast

open AST
open Value
open Asl_utils
open Primops

type sym =
  | Val of value
  | Exp of expr

type 'a sym_pattern =
  | SymPat_LitInt of bitsLit
  | SymPat_LitHex of bitsLit
  | SymPat_LitBits of bitsLit
  | SymPat_LitMask of bitsLit
  | SymPat_Const of ident
  | SymPat_Wildcard
  | SymPat_Tuple of pattern list
  | SymPat_Set of pattern list
  | SymPat_Range of 'a * 'a
  | SymPat_Single of 'a

type 'a sym_expr =
  | SymExpr_If of 'a * 'a * ('a * 'a) list * 'a
  | SymExpr_Binop of 'a * binop * 'a
  | SymExpr_Unop of unop * 'a
  | SymExpr_Field of 'a * ident
  | SymExpr_Fields of 'a * ident list
  | SymExpr_Slices of 'a * ('a * 'a) list
  | SymExpr_In of 'a * 'a sym_pattern
  | SymExpr_Var of ident
  | SymExpr_Parens of 'a
  | SymExpr_Tuple of 'a list
  | SymExpr_Unknown of ty
  | SymExpr_ImpDef of ty * string option
  | SymExpr_TApply of ident * 'a list * 'a list
  | SymExpr_Array of 'a * 'a
  | SymExpr_LitInt of string
  | SymExpr_LitHex of string
  | SymExpr_LitReal of string
  | SymExpr_LitBits of string
  | SymExpr_LitMask of string
  | SymExpr_LitString of string


type sym' =
  | Val' of value
  | Exp' of ty * (sym' sym_expr)

let is_val (x: AST.expr): bool =
    (match x with
    | Expr_LitInt _ -> true
    | Expr_LitBits _ -> true
    | Expr_LitReal _ -> true
    | Expr_LitString _ -> true
    | Expr_Tuple _ -> true
    | x -> false
    )

let rec val_expr (v: Value.value): AST.expr =
  match v with
  | VBool b -> Expr_Var(if b then Ident "TRUE" else Ident "FALSE")
  | VEnum (id, n) -> Expr_LitInt(string_of_int n)
  | VInt n -> Expr_LitInt(Z.to_string n)
  | VReal n -> Expr_LitReal(Q.to_string n)
  | VBits {n; v} -> Expr_LitBits(Z.format ("%0" ^ string_of_int n ^ "b") v)
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

let rec lexpr_to_expr (loc: l) (x: lexpr): expr =
  (match x with
  | LExpr_Var(id) -> Expr_Var(id)
  | LExpr_Field(l,f) -> Expr_Field(lexpr_to_expr loc l,f)
  | LExpr_Array(l,i) -> Expr_Array(lexpr_to_expr loc l,i)
  | _ -> raise (EvalError (Unknown, "unexpected expression in lexpr_to_expr coercion: " ^ pp_lexpr x)))

let filter_uninit (v: value option): value option =
  match v with
  | Some (VUninitialized _) -> None
  | _ -> v

let sym_value_unsafe (x: sym): value =
  match x with
  | Val v -> v
  | Exp e -> failwith ("sym_value_unsafe: required value but got " ^ pp_expr e)

let sym_val_or_uninit (x: sym): value =
  match x with
  | Val v -> v
  | Exp e -> VUninitialized (Type_OfExpr e)

let sym_expr (x: sym): expr =
  match x with
    | Val v -> val_expr v
    | Exp e -> e

let sym_pair_has_exp (pair: sym * sym): bool =
  match pair with
  | Exp _, _ -> true
  | _, Exp _ -> true
  | _ -> false

let sym_initialised (x: sym): sym option =
  match x with
  | Val v -> if val_initialised v then Some x else None
  | Exp _ -> Some x

(** Constructs a sym of a tuple when given a sym for each element in the tuple.
    Result is a Val iff all components are Val.  *)
let rec sym_tuple (syms: sym list): sym =
  match syms with
  | [] -> Val (VTuple [])
  | Val v::rest ->
    (match sym_tuple rest with
    | Val (VTuple vs) -> Val (VTuple (v::vs))
    | Exp (Expr_Tuple es) -> Exp (Expr_Tuple (val_expr v :: es))
    | _ -> failwith "unreachable: sym_tuple should only return tuple in values or expressions.")
  | Exp e::_ -> Exp (Expr_Tuple (List.map sym_expr syms))

let pp_sym (rs: sym): string =
    match rs with
    | Val v -> Printf.sprintf "Val(%s)" (pp_value v)
    | Exp e -> Printf.sprintf "Exp(%s)" (pp_expr e)

let sym_of_tuple (loc: AST.l) (v: sym): sym list  =
  match v with
  | Val (VTuple vs) -> (List.map (fun v -> Val v) vs)
  | Exp (Expr_Tuple vs) -> (List.map (fun v -> Exp v) vs)
  | _ -> raise (EvalError (loc, "tuple expected. Got "^ pp_sym v))

(* Types *)

let type_bool = Type_Constructor(Ident "boolean")
let type_unknown = Type_Constructor(Ident "unknown")

(* Primatives *)

(** Apply a primitive operation to two arguments
    TODO: This will fail to evaluate if the primitive requires a type argument.
          Need to derive this from the arguments somehow.
  *)
let prim_binop (f: string) (loc: AST.l) (x: sym) (y: sym) : sym  =
  (match (x,y) with
  | (Val x,Val y) ->
      (match eval_prim f [] (x::[y]) with
      | Some v -> Val v
      | None -> raise (EvalError (loc, "Unknown primitive operation: "^ f)))
  | (x,y) -> Exp (Expr_TApply(FIdent(f,0), [], (sym_expr x)::[sym_expr y])))

let sym_true     = Val (from_bool true)
let sym_false    = Val (from_bool false)
let sym_eq_int   = prim_binop "eq_int"
let sym_eq_bits  = prim_binop "eq_bits"
let sym_leq      = prim_binop "leq"
let sym_bool_and = prim_binop "bool_and"
let sym_inmask   = prim_binop "in_mask"
let sym_add_int  = prim_binop "add_int"
let sym_sub_int  = prim_binop "sub_int"

let sym_append_bits loc x y =
  (match (x,y) with
  | (Val (VBits {n=0; _}), y) -> y
  | (x, Val (VBits {n=0; _})) -> x
  | (Val (VBits x),Val (VBits y)) -> Val (VBits (prim_append_bits x y))
  | (x,y) -> Exp (Expr_TApply(FIdent("append_bits",0), [Expr_LitInt "-1"; Expr_LitInt "-1"], (sym_expr x)::[sym_expr y])))
(* WARNING: incorrect type arguments passed to append_bits but sufficient for evaluation
   of primitive with eval_prim. *)

let sym_insert_bits loc old i w v =
  match (old, v, i, w) with
  | (Val old', Val v', Val i', Val w') -> Val (insert_bits loc old' i' w' v')
  | _ -> failwith "sym_insert_bits" (* difficult because we need to know widths of each expression. *)

let sym_extract_bits loc v i w =
  match (v, i, w) with
  | (Val v', Val i', Val w') -> Val (extract_bits'' loc v' i' w')
  | _ -> Exp (Expr_Slices (sym_expr v, [Slice_LoWd (sym_expr i, sym_expr w)]))

(* TODO: There is no eval_eq, we need to find the types of x & y *)
let sym_eq (loc: AST.l) (x: sym) (y: sym): sym =
  (match (x,y) with
  | (Val x,Val y) -> Val (from_bool (eval_eq loc x y))
  | (_,_) -> prim_binop "eval_eq" loc x y)

let rec sym_slice (loc: l) (x: sym) (lo: int) (wd: int): sym =
  let int_expr i = Expr_LitInt (string_of_int i) in
  match x with
  | Val v -> Val (extract_bits' loc v lo wd)
  | Exp e ->
    let slice_expr =
      (Expr_Slices (e, [Slice_LoWd (int_expr lo, int_expr wd)])) in
    (match e with
    | (Expr_TApply (FIdent ("append_bits", 0), [Expr_LitInt t1; Expr_LitInt t2], [x1; x2])) ->
      let t2 = int_of_string t2 in
      if (lo >= t2) then
        (* slice is entirely within upper part (i.e. significant bits). *)
        sym_slice loc (Exp x1) (lo - t2) wd
      else if (lo + wd <= t2) then
        (* entirely within lower part. *)
        sym_slice loc (Exp x2) lo wd
      else
        Exp slice_expr
    | _ -> Exp slice_expr)

let sym_concat (loc: AST.l) (xs: sym list): sym =
  match xs with
  | [] -> Val (VBits empty_bits)
  | x::xs -> List.fold_left (sym_append_bits loc) x xs

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
  | ("add_int",     _,                [Val x1; x2])       when is_zero x1 -> Some x2
  | ("add_int",     _,                [x1; Val x2])       when is_zero x2 -> Some x1
  | ("append_bits", [Val t1; _],      [_; x2])            when is_zero t1 -> Some x2
  | ("append_bits", [_; Val t2],      [x1; _])            when is_zero t2 -> Some x1
  | ("or_bits",     _,                [Val x1; x2])       when is_zero_bits x1 -> Some x2
  | ("or_bits",     _,                [x1; Val x2])       when is_zero_bits x2 -> Some x1
  | _ -> None)

let rec val_type (v: value): ty =
  let unsupported () = failwith @@ "val_type unsupported: " ^ pp_value v in
  match v with
  | VBool _ -> type_builtin "boolean"
  | VEnum (ident, _) -> Type_Constructor ident
  | VInt _ -> type_builtin "integer"
  | VReal _ -> type_builtin "real"
  | VBits {n=n; _} -> type_bits (string_of_int n)
  | VMask mask -> unsupported ()
  | VString _ -> type_builtin "string"
  | VExc _ -> type_builtin "__Exception"
  | VTuple vs -> Type_Tuple (List.map val_type vs)
  | VRecord (_) -> unsupported ()
  | VArray (arr, def) -> unsupported ()
  | VRAM _ -> type_builtin "__RAM"
  | VUninitialized ty -> ty

let sym_type =
  function
  | Val v -> val_type v
  | Exp e -> Type_OfExpr e (* FIXME: add type annotation to sym Exp constructor. *)

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

(** Structure to represent a chain of reference expressions *)

type access_chain =
  | Field of ident
  | Index of value

let rec get_access_chain (loc: l) (v: value) (a: access_chain list) : value =
  (match a with
  | (Field f)::a -> get_field loc (get_access_chain loc v a) f
  | (Index i)::a -> get_array loc (get_access_chain loc v a) i
  | [] -> v)

let rec lexpr_access_chain (x: lexpr) (a: access_chain list): lexpr =
  (match a with
  | (Field f)::a -> lexpr_access_chain (LExpr_Field(x,f)) a
  | (Index i)::a -> lexpr_access_chain (LExpr_Array(x,val_expr i)) a
  | [] -> x)

let rec expr_access_chain (x: expr) (a: access_chain list): expr =
  (match a with
  | (Field f)::a -> expr_access_chain (Expr_Field(x,f)) a
  | (Index i)::a -> expr_access_chain (Expr_Array(x,val_expr i)) a
  | [] -> x)
