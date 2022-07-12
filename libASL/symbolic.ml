module AST = Asl_ast

open AST
open Value

type sym =
  | Val of value
  | Exp of expr

let rec to_expr (v: Value.value): AST.expr = 
  match v with 
  | VBool b -> Expr_LitInt(if b then "1" else "0")
  | VEnum (id, n) -> Expr_LitInt(string_of_int n)
  | VInt n -> Expr_LitInt(Z.to_string n)
  | VReal n -> Expr_LitReal(Q.to_string n)
  | VBits {n; v} -> Expr_LitInt(Z.to_string v)
  | VString s -> Expr_LitString(s)
  | VTuple vs -> Expr_Tuple(List.map to_expr vs)
  | _ -> raise (EvalError (Unknown, "Casting unhandled value type to expression"))

let sym_expr (x: sym): expr = 
  match x with
    | Val v -> to_expr v
    | Exp e -> e

let sym_binop (op: binop) (f: value -> value -> value) (x: sym) (y: sym): sym = 
  match (x,y) with
  | (Val x, Val y) -> Val (f x y)
  | (x, y) -> Exp (Expr_Binop(sym_expr x, op, sym_expr y))

let sym_add_int (loc: l): sym -> sym -> sym = 
  sym_binop Binop_Plus (eval_add_int loc)

let sym_sub_int (loc: l): sym -> sym -> sym = 
  sym_binop Binop_Minus (eval_sub_int loc)