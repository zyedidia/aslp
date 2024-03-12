open Asl_ast
open Asl_utils

type st = {
  mutable depth : int;
  mutable skip_seq : bool;
  oc : out_channel;
  mutable ref_vars : IdentSet.t;
}

let inc_depth st =
  st.depth <- st.depth + 2
  
let dec_depth st =
  st.depth <- st.depth - 2

let write_preamble env st =
  Printf.fprintf st.oc "open Gen_prelude\n\n"



let replace s =
  String.fold_left (fun acc c ->
    if c = '.' then acc ^ "_"
    else if c = '#' then acc ^ "HASH"
    else acc ^ (String.make 1 c)) "" s

let name_of_ident v =
  let s = (match v with
  | Ident n -> "v_" ^ n
  | FIdent (n,0) -> "f_" ^ n 
  | FIdent (n,i) -> "f_" ^ n ^ "_" ^ (string_of_int i)) in
  replace s

let is_ref_var v st = IdentSet.mem v st.ref_vars

let clear_ref_vars st = st.ref_vars <- IdentSet.empty

let add_ref_var v st = st.ref_vars <- IdentSet.add v st.ref_vars

let rec prints_expr e st =
  match e with
  | Expr_If(_, c, t, els, f) ->
      let rec iter = (function
        | (E_Elsif_Cond(c, t)::xs) ->
            let c = prints_expr c st in
            let t = prints_expr t st in
            let f = iter xs in
            Printf.sprintf "if %s then %s else %s" c t f
        | [] -> prints_expr f st) in
      iter (E_Elsif_Cond(c, t)::els)
  | Expr_Field(e, f) ->
      let e = prints_expr e st in
      e ^ "." ^ name_of_ident f
  | Expr_Var(v) -> 
      let n = name_of_ident v in
      if is_ref_var v st then "!" ^ n
      else n

  | Expr_Slices(e,[Slice_LoWd(i,w)]) ->
      let e = prints_expr e st in
      let i = prints_expr i st in
      let w = prints_expr w st in
      Printf.sprintf "extract_bits (%s) (%s) (%s)" e i w

  | Expr_Slices(e,(Slice_LoWd(i,w)::xs)) ->
      let e' = prints_expr e st in
      let i = prints_expr i st in
      let w = prints_expr w st in
      let r = prints_expr (Expr_Slices(e,xs)) st in
      Printf.sprintf "prim_append (extract_bits (%s) (%s) (%s)) (%s)" e' i w r
  | Expr_TApply(FIdent("and_bool", 0), [], [a;b]) ->
      let a = prints_expr a st in
      let b = prints_expr b st in
      "(" ^ a ^ ") && (" ^ b ^ ")"
  | Expr_TApply(FIdent("or_bool", 0), [], [a;b]) ->
      let a = prints_expr a st in
      let b = prints_expr b st in
      "(" ^ a ^ ") && (" ^ b ^ ")"

  | Expr_TApply(f, targs, args) ->
      let f = name_of_ident f in
      let args = List.map (fun e -> prints_expr e st) (targs @ args) in
      f ^ " (" ^ (String.concat ") (" args) ^ ")"
  | Expr_Tuple(es) -> "(" ^ (String.concat "," (List.map (fun e -> prints_expr e st) es)) ^ ")"

  | Expr_Unknown(ty) -> default_value ty st

  | Expr_In(e,Pat_LitMask(l)) ->
      let  e = prints_expr e st in
      Printf.sprintf "f_in_mask (%s) (from_maskLit (\"%s\"))" e l

  | Expr_LitInt i -> "Z.of_string \"" ^ i ^ "\""
  | Expr_LitString s -> "\"" ^ s ^ "\""
  | Expr_LitBits b -> "from_bitsLit \"" ^ b ^ "\""
  | Expr_LitReal(r) -> "from_realLit \"" ^ r ^ "\""

  | Expr_Array(a,i) ->
      let a = prints_expr a st in
      let i = prints_expr i st in
      Printf.sprintf "List.nth (%s) (%s)" a i

  | Expr_ImpDef(_,Some(s)) ->
      Printf.sprintf "impdef \"%s\"" s

  | _ -> 
      Printf.printf "prints_expr: %s\n" (pp_expr e);
      "TODO(expr)"

and default_value t st =
  match t with
  | Type_Bits w ->
      let w = prints_expr w st in
      Printf.sprintf "mkBits (%s) Z.zero" w
  | Type_Constructor (Ident "boolean") ->
      "true"
  | Type_Constructor (Ident "integer") ->
      "Z.zero"
  | Type_Constructor (Ident "real") ->
      "Q.zero"
  | Type_Array(Index_Range(lo, hi),ty) ->
      let lo = prints_expr lo st in
      let hi = prints_expr hi st in
      let d = default_value ty st in
      Printf.sprintf "List.init ((Z.to_int (%s)) - (Z.to_int (%s)) + 1) (fun _ -> %s)" hi lo d
  | Type_Constructor (Ident "__RAM") -> ""
  | Type_Constructor (Ident "rt_label") ->
      "0"
  | Type_Constructor (Ident "rt_sym") ->
      "0"
  | _ -> failwith @@ "Unknown type for default value: " ^ (pp_type t)

let write_line s st =
  let padding = String.concat "" (List.init st.depth (fun _ -> " ")) in
  Printf.fprintf st.oc  "%s%s" padding s

let write_seq st =
  if st.skip_seq then 
    st.skip_seq <- false
  else Printf.fprintf st.oc ";\n"

let write_nl st =
  Printf.fprintf st.oc "\n"

let write_val_return e st =
  let s = Printf.sprintf "(%s)" e in
  write_line s st

let write_return st =
  write_line "()" st

let write_ref v e st =
  let name = name_of_ident v in
  let s = Printf.sprintf "let %s = ref (%s) in\n" name e in
  st.skip_seq <- true;
  write_line s st;
  add_ref_var v st

let write_let v e st =
  let v = name_of_ident v in
  let s = Printf.sprintf "let %s = %s in\n" v e in
  st.skip_seq <- true;
  write_line s st

let rec name_of_lexpr l =
  match l with
  | LExpr_Var v -> name_of_ident v
  | LExpr_Field (l, f) ->
      let l = name_of_lexpr l in
      let f = name_of_ident f in
      l ^ "." ^ f
  | LExpr_Wildcard -> "_"
  | _ -> failwith @@ "name_of_lexpr: " ^ (pp_lexpr l)

let rec write_assign v e st =
  match v with
  | LExpr_Var v ->
      let v = name_of_ident v in
      let s = Printf.sprintf "%s := %s" v e in
      write_line s st

  | LExpr_Array (LExpr_Var v, i) ->
      let i = prints_expr i st in
      let v = name_of_ident v in
      let s = Printf.sprintf "%s := list_update (%s) (%s) (%s)" v v i e in
      write_line s st

  | LExpr_Field (l, f) ->
      let v = name_of_lexpr l in
      let s = Printf.sprintf "%s = %s" v e in
      write_line s st

  | LExpr_Tuple (ls) ->
      let vars = List.init (List.length ls) (fun i -> "tmp" ^ (string_of_int i)) in
      let v = "(" ^ String.concat "," vars ^ ")" in
      let s = Printf.sprintf "let %s = %s in\n" v e in
      st.skip_seq <- true;
      write_line s st;
      List.iter2 (fun l e ->
        write_seq st;
        write_assign l e st
      ) ls vars

  | LExpr_Wildcard ->
      let s = Printf.sprintf "let _ = %s in\n" e in
      st.skip_seq <- true;
      write_line s st

  | _ -> failwith @@ "Unknown lexpr: " ^ (pp_lexpr v)

let write_if_start c st =
  let s = Printf.sprintf "if %s then begin\n" c in
  write_line s st

let write_if_elsif c st =
  write_nl st;
  let s = Printf.sprintf "end else if %s then begin\n" c in
  write_line s st

let write_if_else st =
  write_nl st;
  write_line "end else begin\n" st

let write_if_end st =
  write_nl st;
  write_line "end" st

let write_while_start c st =
  let s = Printf.sprintf "while %s do\n" c in
  write_line s st

let write_while_end st =
  write_nl st;
  write_line "done" st

let write_assert s st =
  if s = "v_FALSE" then
    let s = Printf.sprintf "failwith \"unsupported\"" in
    write_line s st
  else
    let s = Printf.sprintf "assert (%s)" s in
    write_line s st

let write_call f targs args st =
  let f = name_of_ident f in
  let args = targs @ args in
  let call = f ^ " (" ^ (String.concat ") (" args) ^ ")" in
  write_line call st

let negate c =
  Printf.sprintf "not (%s)" c

let rec write_stmt s st =
  match s with
  | Stmt_VarDeclsNoInit(ty, vs, loc) -> 
      let e = default_value ty st in
      List.iter (fun v -> write_ref v e st) vs

  | Stmt_VarDecl(ty, v, e, loc) ->
      let e = prints_expr e st in
      write_ref v e st

  | Stmt_ConstDecl(ty, v, e, loc) ->
      let e = prints_expr e st in
      write_let v e st

  | Stmt_Assign(l, r, loc) ->
      let e = prints_expr r st in
      write_assign l e st

  | Stmt_TCall(f, tes, es, loc) ->
      let tes = List.map (fun e -> prints_expr e st) tes in
      let es = List.map (fun e -> prints_expr e st) es in
      write_call f tes es st

  | Stmt_FunReturn(e, loc) ->
      let e = prints_expr e st in
      write_val_return e st

  | Stmt_ProcReturn(loc) ->
      write_return st

  | Stmt_If(c, t, [], [], loc) ->
      let c = prints_expr c st in
      write_if_start c st;
      inc_depth st;
      write_stmts t st;
      if st.skip_seq then (write_return st; st.skip_seq <- false);
      dec_depth st;
      write_if_end st

  | Stmt_If(c, [], [], t, loc) ->
      let c = prints_expr c st in
      let c = negate c in
      write_if_start c st;
      inc_depth st;
      write_stmts t st;
      if st.skip_seq then (write_return st; st.skip_seq <- false);
      dec_depth st;
      write_if_end st

  | Stmt_If(c, t, els, f, loc) ->
      let rec iter = function
      | S_Elsif_Cond(c,b)::xs ->
          let c = prints_expr c st in
          write_if_elsif c st;
          inc_depth st;
          write_stmts b st;
          if st.skip_seq then (write_return st; st.skip_seq <- false);
          dec_depth st;
          iter xs
      | [] -> () in

      (* True branch *)
      assert (List.length t <> 0);
      let c = prints_expr c st in
      write_if_start c st;
      inc_depth st;
      write_stmts t st;
      if st.skip_seq then (write_return st; st.skip_seq <- false);
      dec_depth st;

      (* Else cases *)
      iter els;

      (* Default branch *)
      assert (List.length f <> 0);
      write_if_else st;
      inc_depth st;
      write_stmts f st;
      if st.skip_seq then (write_return st; st.skip_seq <- false);
      dec_depth st;
      write_if_end st

  | Stmt_Assert(e, loc) ->
      let e = prints_expr e st in
      write_assert e st

  | Stmt_Throw _ 
  | Stmt_Unpred _ 
  | Stmt_Dep_Unpred _
  | Stmt_Dep_ImpDef _
  | Stmt_Dep_Undefined _
  | Stmt_Undefined _
  | Stmt_ConstrainedUnpred _ ->
      let s = Printf.sprintf "failwith \"unsupported\"" in
      write_line s st
 
  | Stmt_While(b, c, loc) ->
      let b = prints_expr b st in
      write_while_start b st;
      inc_depth st;
      write_stmts c st;
      (* if we get here with seq skip, means we just introduce a let for no reason *)
      if st.skip_seq then (write_return st; st.skip_seq <- false);
      dec_depth st;
      write_while_end st

  | _ -> failwith @@ "write_stmt: " ^ (pp_stmt s);

and write_stmts s st =
  match s with
  | [] -> failwith "write_stmts of empty list"
  | x::xs -> 
      write_stmt x st;
      List.iter (fun s -> 
        write_seq st;
        write_stmt s st
      ) xs

let build_args targs args =
  if List.length targs = 0 && List.length args = 0 then "()"
  else String.concat " " (List.map name_of_ident (targs@args))
 
let write_fn name (ret_tyo,_,targs,args,_,body) st =
  if List.length body = 0 then () 
  else
    let args = build_args targs args in
    Printf.fprintf st.oc "let %s %s = \n" (name_of_ident name) args;
    inc_depth st;
    write_stmts body st;
    dec_depth st;
    clear_ref_vars st;
    Printf.fprintf st.oc "\n\n"

let write_epilogue fid env st =
  Printf.fprintf st.oc "let run enc =\n  clear_res ();\n  %s enc;\n  get_res ()\n" (name_of_ident fid)

let run fid fns env (filename: string) =
  let oc = open_out filename in
  let st = { depth = 0; skip_seq = false; oc ; ref_vars = IdentSet.empty } in
  write_preamble env st;
  let dsig = Bindings.find fid fns in
  let fns = Bindings.remove fid fns in
  Bindings.iter (fun k b -> write_fn k b st) fns;
  write_fn fid dsig st;
  write_epilogue fid env st;
  close_out oc;

