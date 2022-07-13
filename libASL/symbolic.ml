module AST = Asl_ast

open AST
open Value

type sym =
  | Val of value
  | Exp of expr

let sym_true = Val (from_bool true)
let sym_false = Val (from_bool true)

let rec val_expr (v: Value.value): AST.expr = 
  match v with 
  | VBool b -> Expr_LitInt(if b then "1" else "0")
  | VEnum (id, n) -> Expr_LitInt(string_of_int n)
  | VInt n -> Expr_LitInt(Z.to_string n)
  | VReal n -> Expr_LitReal(Q.to_string n)
  | VBits {n; v} -> Expr_LitInt(Z.to_string v)
  | VString s -> Expr_LitString(s)
  | VTuple vs -> Expr_Tuple(List.map val_expr vs)
  | _ -> raise (EvalError (Unknown, "Casting unhandled value type to expression"))

let val_opt_initialised (v: value option): value option =
  match v with
  | Some VUninitialized -> None
  | _ -> v

let sym_val_or_uninit (x: sym): value = 
  match x with
  | Val v -> v
  | Exp _ -> VUninitialized

let sym_expr (x: sym): expr = 
  match x with
    | Val v -> val_expr v
    | Exp e -> e

let sym_binop (op: binop) (f: value -> value -> value) (x: sym) (y: sym): sym = 
  match (x,y) with
  | (Val x, Val y) -> Val (f x y)
  | (x, y) -> Exp (Expr_Binop(sym_expr x, op, sym_expr y))

let sym_add_int (loc: l): sym -> sym -> sym = 
  sym_binop Binop_Plus (eval_add_int loc)

let sym_sub_int (loc: l): sym -> sym -> sym = 
  sym_binop Binop_Minus (eval_sub_int loc)

let sym_pair_has_exp (pair: sym * sym): bool = 
  match pair with
  | Exp _, _ -> true
  | _, Exp _ -> true
  | _ -> false

(** Deconstructs the given list of symbolics.
    Returns a Right of values if the entire list was concrete values,
    otherwise returns a Left of everything coerced to expressions. *)
let rec sym_collect_list (xs: sym list): (expr list, value list) Either.t =
  match xs with
  | [] -> Right []
  | Val v::xs -> 
    (match sym_collect_list xs with
    | Right rs -> Right (v::rs)
    | Left ls -> Left ((val_expr v)::ls))
  | Exp e::xs -> Left (e :: List.map sym_expr xs)