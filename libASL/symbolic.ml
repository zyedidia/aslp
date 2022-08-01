module AST = Asl_ast

module TC = Tcheck

open AST
open Value
open Asl_utils

type sym =
  | Val of ty * value
  | Exp of ty * expr

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


let rec val_type (v: value): ty =
  let unsupported () = failwith @@ "val_type unsupported: " ^ pp_value v in
  match v with
  | VBool _ -> TC.type_bool
  | VEnum (ident, _) -> Type_Constructor ident
  | VInt _ -> TC.type_integer
  | VReal _ -> TC.type_real
  | VBits {n=n; _} -> TC.type_bitsN n
  | VMask mask -> unsupported ()
  | VString _ -> TC.type_string
  | VExc _ -> TC.type_exn
  | VTuple vs -> Type_Tuple (List.map val_type vs)
  | VRecord (_) -> unsupported ()
  | VArray (arr, def) -> unsupported ()
  | VRAM _ -> Type_Constructor (Ident "__RAM")
  | VUninitialized ty -> ty

let sym_uninit (t: ty) = Val (t, VUninitialized t)

let sym_type = function
  | Val (t,_) -> t
  | Exp (t,_) -> t


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
  | VBits {n; v} -> Expr_LitBits(Z.to_string v)
  | VString s -> Expr_LitString(s)
  | VTuple vs -> Expr_Tuple(List.map val_expr vs)
  | _ -> raise (EvalError (Unknown, "Casting unhandled value type to expression"))

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
  | Exp (t, e) -> VUninitialized t

let sym_expr (x: sym): expr =
  match x with
    | Val (_,v) -> val_expr v
    | Exp (_,e) -> e

let sym_pair_has_exp (pair: sym * sym): bool =
  match pair with
  | Exp _, _ -> true
  | _, Exp _ -> true
  | _ -> false

let sym_initialised (x: sym): sym option =
  match x with
  | Val (_, VUninitialized _) -> None
  | _ -> Some x

(** Deconstructs the given list of symbolics.
    Returns a Right of values if the entire list was concrete values,
    otherwise returns a Left of everything coerced to expressions. *)
let rec sym_collect_list (xs: sym list): (expr list, value list) Either.t =
  match xs with
  | [] -> Right []
  | Val (_,v)::xs ->
    (match sym_collect_list xs with
    | Right rs -> Right (v::rs)
    | Left ls -> Left ((val_expr v)::ls))
  | Exp (_,e)::xs -> Left (e :: List.map sym_expr xs)

let pp_sym (rs: sym): string =
    match rs with
    | Val (t,v) -> Printf.sprintf "Val[%s](%s)" (pp_type t) (pp_value v)
    | Exp (t,e) -> Printf.sprintf "Exp[%s](%s)" (pp_type t) (pp_expr e)

let sym_of_tuple (loc: AST.l) (v: sym): sym list  =
  match v with
  | Val (Type_Tuple ts,(VTuple vs)) -> (List.map2 (fun t v -> Val (t,v)) ts vs)
  | Exp (Type_Tuple ts,(Expr_Tuple vs)) -> (List.map2 (fun t v -> Exp (t,v)) ts vs)
  | _ -> raise (EvalError (loc, "tuple expected. Got "^ pp_sym v))

(* Primitives *)

(** Apply a primitive operation to two arguments
    TODO: This will fail to evaluate if the primitive requires a type argument.
          Need to derive this from the arguments somehow.
  *)
let prim_binop (f: string) (loc: AST.l) (x: sym) (y: sym) : sym  =
  (match (x,y) with
  | (Val (tx,x),Val (ty,y)) ->
      (match eval_prim f [] (x::[y]) with
      | Some v -> Val (val_type v, v)
      | None -> raise (EvalError (loc, "Unknown primitive operation: "^ f)))
  | (x,y) -> Exp (Type_Tuple [], Expr_TApply(FIdent(f,0), [], (sym_expr x)::[sym_expr y])))

let sym_true     = Val (TC.type_bool, from_bool true)
let sym_false    = Val (TC.type_bool, from_bool false)
let sym_eq_int   = prim_binop "eq_int"
let sym_eq_bits  = prim_binop "eq_bits"
let sym_leq      = prim_binop "leq"
let sym_bool_and = prim_binop "bool_and"
let sym_inmask   = prim_binop "in_mask"
let sym_add_int  = prim_binop "add_int"
let sym_sub_int  = prim_binop "sub_int"

(* TODO: There is no eval_eq, we need to find the types of x & y *)
let sym_eq (loc: AST.l) (x: sym) (y: sym): sym =
  (match (x,y) with
  | (Val (_,x),Val (_,y)) -> Val (TC.type_bool, from_bool (eval_eq loc x y))
  | (_,_) -> prim_binop "eval_eq" loc x y)

let rec sym_slice (loc: l) (x: sym) (lo: int) (wd: int): sym =
  let t = TC.type_bitsN wd in
  let int_expr i = Expr_LitInt (string_of_int i) in
  match x with
  | Val (_,v) -> Val (t, extract_bits' loc v lo wd)
  | Exp (_,e) ->
    let slice_expr =
      (Expr_Slices (e, [Slice_LoWd (int_expr lo, int_expr wd)])) in
    (match e with
    | (Expr_TApply (FIdent ("append_bits", 0), [Expr_LitInt t1; Expr_LitInt t2], [x1; x2])) ->
      let t2 = int_of_string t2 in
      if (lo >= t2) then
        (* slice is entirely within upper part (i.e. significant bits). *)
        sym_slice loc (Exp (t,x1)) (lo - t2) wd
      else if (lo + wd <= t2) then
        (* entirely within lower part. *)
        sym_slice loc (Exp (t,x2)) lo wd
      else
        Exp (t,slice_expr)
    | _ -> Exp (t,slice_expr))

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
  | ("add_int",     _,                [Val (_,x1); x2])       when is_zero x1 -> Some x2
  | ("add_int",     _,                [x1; Val (_,x2)])       when is_zero x2 -> Some x1
  | ("append_bits", [Val (_,t1); _],  [_; x2])                when is_zero t1 -> Some x2
  | ("append_bits", [_; Val (_,t2)],  [x1; _])                when is_zero t2 -> Some x1
  | ("or_bits",     _,                [Val (_,x1); x2])       when is_zero_bits x1 -> Some x2
  | ("or_bits",     _,                [x1; Val (_,x2)])       when is_zero_bits x2 -> Some x1
  | _ -> None)
