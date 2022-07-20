module AST = Asl_ast

open AST
open Value
open Asl_utils

type sym =
  | Val of value
  | Exp of expr

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

let sym_pair_has_exp (pair: sym * sym): bool = 
  match pair with
  | Exp _, _ -> true
  | _, Exp _ -> true
  | _ -> false

let sym_initialised (x: sym): sym option = 
  match x with
  | Val VUninitialized -> None
  | _ -> Some x

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

(* TODO: There is no eval_eq, we need to find the types of x & y *)
let sym_eq (loc: AST.l) (x: sym) (y: sym): sym =
  (match (x,y) with
  | (Val x,Val y) -> Val (from_bool (eval_eq loc x y))
  | (_,_) -> prim_binop "eval_eq" loc x y)

let rec contains_uninit (v: value): bool =
  match v with 
  | VUninitialized -> true
  | VTuple vs -> List.exists contains_uninit vs
  | VRecord r -> Bindings.exists (fun _ -> contains_uninit) r
  | _ -> false
