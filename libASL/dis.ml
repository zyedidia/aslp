(****************************************************************
 * ASL dissassembler
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL dissassembler *)

module PP   = Asl_parser_pp
module AST  = Asl_ast
module TC   = Tcheck

open AST
open Asl_utils
open Value
open Eval

open Symbolic

let debug_level = ref 0

(** (name, arg, location) tuple for tracing disassembly calls.
    For example: ("dis_expr", "1+1", loc).
*)
type dis_trace = (string * string * l) list

exception DisTrace of dis_trace * exn
exception DisUnsupported of l * string
exception DisInternalError of l * string

let unsupported (loc: l) (msg: string) =
  raise (DisUnsupported (loc, msg))

let print_dis_trace (trace: dis_trace) =
    String.concat "\n" @@ List.map (fun (f, e, l) ->
        Printf.sprintf "... at %s: %s --> %s" (pp_loc l) f e)
        (trace)

let () = Printexc.register_printer
    (function
    | DisTrace (trace, exn) ->
        let trace' =
            if !debug_level >= 1
            then "\n" ^ print_dis_trace trace
            else ""
        in
        Some (Printexc.to_string exn ^ "\n" ^ trace')
    | DisUnsupported (loc, s) ->
        Some ("DisUnsupported: " ^ pp_loc loc ^ ": " ^ s)
    | Value.Throw (loc, e) ->
        Some ("LibASL.Value.Throw(" ^ Primops.pp_exc e ^ ") at " ^ pp_loc loc)
    | _ -> None)


module LocalEnv = struct
    type t = {
        locals          : (ty * sym) Bindings.t list;
        returnSymbols   : expr list;
        numSymbols      : int;
        indent          : int;
        trace           : dis_trace;
    }

    let empty () =
        {
            locals = [Bindings.empty];
            returnSymbols = [];
            numSymbols = 0;
            indent = 0;
            trace = [];
        }

    let sequence_merge (first: t) (second: t): t =
        {
            first with numSymbols = max first.numSymbols second.numSymbols
        }

    let join_merge (l: t) (r: t): t =
        assert (l.returnSymbols = r.returnSymbols);
        assert (l.indent = r.indent);
        assert (l.trace = r.trace);

        let merge_bindings l r =
            Bindings.fold (fun k (t1,v1) bs ->
                match Bindings.find_opt k r with
                | None -> bs
                | Some (t2,v2) ->
                    let v = if v1 = v2 then v1 else Val (VUninitialized (sym_type v1)) in
                    Bindings.add k (t1,v) bs)
            l Bindings.empty in
        let locals' = List.map2 merge_bindings l.locals r.locals in
        {
            locals = locals';
            returnSymbols = l.returnSymbols;
            numSymbols = max l.numSymbols r.numSymbols;
            indent = l.indent;
            trace = l.trace;
        }

    let pp_value_bindings = Utils.pp_list (pp_bindings pp_value)

    let pp_sym_bindings (bss: (ty * sym) Bindings.t list) =
        Utils.pp_list (pp_bindings (fun (_,e) -> pp_sym e)) bss

    let pp_locals (env: t): string =
        Printf.sprintf "locals = %s" (pp_sym_bindings env.locals)

    let getReturnSymbol (loc: l) (env: t): expr =
        match env.returnSymbols with
        | [] -> raise (EvalError (loc, "Return not in function"))
        | (e :: rs) -> e

    let addReturnSymbol (e: expr) (env: t): t =
        {env with returnSymbols = e :: env.returnSymbols}

    let removeReturnSymbol (env: t): t =
        match env.returnSymbols with
        | [] -> env
        | (s::ss) -> {env with returnSymbols = ss}

    let getNumSymbols (env: t): int =
        env.numSymbols

    let incNumSymbols (env: t): int * t =
        let env' = {env with numSymbols = env.numSymbols + 1} in
        (env'.numSymbols, env')

    (* TODO: 0 should not map to "" *)
    let getLocalPrefix (env: t): string =
        string_of_int (List.length env.locals)

    let getLocalName (x: ident) (env: t): ident =
        Ident (pprint_ident x ^ "__" ^ getLocalPrefix env)

    let addLevel (env: t): t =
        {env with locals = (Bindings.empty)::env.locals}

    let popLevel (env: t): t =
        match env.locals with
        | [] -> raise (EvalError (Unknown, "No levels exist"))
        | (_::ls) -> {env with locals = ls}

    let rec search (x: ident) (bss : 'a Bindings.t list): 'a Bindings.t option =
        match bss with
        | (bs :: bss') ->
            if Bindings.mem x bs then Some bs else search x bss'
        | [] -> None

    let addLocalVar (loc: l) (k: ident) (v: sym) (t: ty) (env: t): t =
        if !trace_write then Printf.printf "TRACE: fresh %s = %s\n" (pprint_ident k) (pp_sym v);
        match env.locals with
        | (bs :: rest) -> {env with locals = (Bindings.add k (t,v) bs :: rest)}
        | []        -> raise (EvalError (loc, "LocalEnv::addLocalVar bindings empty"))

    let addLocalConst = addLocalVar

    let getLocalVar (loc: l) (x: ident) (env: t): (ty * sym) option =
        (match (search x env.locals) with
        | Some bs -> Some (Bindings.find x bs)
        | None -> None)

    let setLocalVar (loc: l) (x: ident) (v: sym) (env: t): t =
        if !trace_write then Printf.printf "TRACE: write %s = %s\n" (pprint_ident x) (pp_sym v);
        let locals' = Utils.replace_in_list (fun b ->
          match Bindings.find_opt x b with
          | Some (t,_) -> Some (Bindings.add x (t,v) b)
          | None -> None
        ) env.locals in
        { env with locals = locals' }

    let trySetLocalVar (loc: l) (x: ident) (v: sym) (env: t): t =
        try setLocalVar loc x v env
        with Not_found -> env
end

module DisEnv = struct
    include Rws.Make(struct
        type r = Env.t
        type w = stmt list
        type s = LocalEnv.t
        let mempty = []
        let mappend = (@)
    end)

    open Let

    let catch (f: 'b -> 'a) (x: 'b): 'a option =
        try Some (f x)
        with EvalError _ -> None

    let getVar (loc: l) (x: ident): sym rws =
        let* v = gets (LocalEnv.getLocalVar loc x) in
        match (v) with
            | Some (_,v) -> pure (v)
            | None -> reads (fun env -> Val (Env.getVar loc env x))

    let getVarOpt (loc: l) (x: ident): sym option rws =
        let* v = gets (LocalEnv.getLocalVar loc x) in
        match (v) with
            | Some (_,v) -> pure (Some v)
            | None -> reads (catch (fun env -> Val (Env.getVar loc env x)))

    let getLocalName (x: ident): ident rws =
        gets (LocalEnv.getLocalName x)

    let findVar (loc: l) (id: ident): ident option rws =
        let* localid = gets (LocalEnv.getLocalName id) in
        let* v = getVarOpt loc localid in
        match v with
        | Some _ -> pure (Some localid)
        | None ->
            let+ v = getVarOpt loc id in
            (match v with
            | None -> None
            | Some _ -> Some id)

    let getFun (loc: l) (x: ident): fun_sig option rws =
        reads (catch (fun env -> Env.getFun loc env x))

    let nextVarName (prefix: string): ident rws =
        let+ num = stateful LocalEnv.incNumSymbols in
        Ident (prefix ^ string_of_int num)

    let indent: string rws =
        let+ i = gets (fun l -> l.indent) in
        let h = i / 2 in
        let s = String.concat "" (List.init h (fun _ -> "\u{2502} \u{250a} ")) in
        if i mod 2 == 0 then
            s ^ ""
        else
            s ^ "\u{2502} "

    let debug (minLevel: int) (s: string): unit rws =
        if !debug_level >= minLevel then
            let+ i = indent in
            let s' = Str.global_replace (Str.regexp "\n") ("\n"^i) s in
            Printf.printf "%s%s\n" i s';
            ()
        else
            unit

    let log (s: string): unit rws =
        debug 1 s

    let warn s = debug 0 ("WARNING: " ^ s)

    let scope (loc: l) (name: string) (arg: string) (pp: 'a -> string) (x: 'a rws): 'a rws =
        (* logging header. looks like: +- dis_expr --> 1 + 1. *)
        log (Printf.sprintf "\u{256d}\u{2500} %s --> %s" name arg) >>

        (* add indentation level for logging. *)
        modify (fun l -> {l with indent = l.indent + 1}) >>
        modify (fun l -> {l with trace = (name,arg,loc)::l.trace}) >>
        let* trace = gets (fun l -> l.trace) in

        (* run computation but obtain state and writer to output in debugging. *)
        let* (result,s',w') = locally (catcherror x) in
        let x' = (match result with
        | Error ((DisTrace _) as e, bt) -> Printexc.raise_with_backtrace e bt
        | Error (exn, bt) -> Printexc.raise_with_backtrace (DisTrace (trace, exn)) bt
        | Ok x' -> x') in
        (* restore state and writer. *)
        write w' >>
        put s' >>
        (* remove indentation level. *)
        modify (fun l -> {l with indent = l.indent - 1}) >>
        modify (fun l -> {l with trace = List.tl l.trace}) >>

        (* logging footer. *)
        log (Printf.sprintf "\u{2570}\u{2500} = %s" (pp x')) >>
        let* () = if !debug_level >= 2
            then log (Printf.sprintf "   %s\n" (LocalEnv.pp_locals s'))
            else unit
        and* () = if !debug_level >= 3
            then traverse_ (fun s -> log ("   " ^ pp_stmt s)) w' >> log ""
            else unit
        in
        pure x'
end

type 'a rws = 'a DisEnv.rws

let (let@) = DisEnv.Let.(let*)
let (and@) = DisEnv.Let.(and*)
let (let+) = DisEnv.Let.(let+)
let (and+) = DisEnv.Let.(and+)

let (>>) = DisEnv.(>>)
let (>>=) = DisEnv.(>>=)

(** Convert value to a simple expression containing that value, so we can
    print it or use it symbolically *)
let to_expr = sym_expr

(** Converts a result_or_simplified to a value.
    Raises an exception if an expression is given, as an expression cannot be casted to a value.
    Requires checking beforehand *)
let to_value (v: sym): value =
    match v with
    | Val v' -> v'
    | Exp _ -> raise (EvalError (Unknown, "Unreachable"))

let is_expr (v: sym): bool =
    match v with
    | Val _ -> false
    | Exp _ -> true

let contains_expr (xs: sym list): bool =
    List.exists is_expr xs

let getGlobalConst(c: ident): sym rws =
  DisEnv.reads (fun env -> Val (Env.getGlobalConst env c))

let check_var_shadowing (loc: l) (i: ident): unit rws =
    let@ old = DisEnv.gets (LocalEnv.getLocalVar loc i) in
    (match old with
    | Some _ -> (*DisEnv.warn ("shadowing local variable: " ^ pprint_ident i)*)
        DisEnv.unit
    | None -> DisEnv.unit)

let declare_var (loc: l) (t: ty) (i: ident): unit rws =
  let@ i' = DisEnv.getLocalName i in
  check_var_shadowing loc i >>
  DisEnv.modify
    (LocalEnv.addLocalVar loc i (Val (VUninitialized t)) t) >>
  DisEnv.write [Stmt_VarDeclsNoInit(t, [i'], loc)]

let declare_assign_var (loc: l) (t: ty) (i: ident) (x: sym): unit rws =
  let@ i' = DisEnv.getLocalName i in
  check_var_shadowing loc i >>
  DisEnv.modify
    (LocalEnv.addLocalVar loc i x t) >>
  DisEnv.write [Stmt_VarDecl(t, i', sym_expr x, loc)]

let declare_fresh_named_var (loc: l) (name: string)  (t: ty): ident rws =
  let@ res = DisEnv.nextVarName name in
  let+ () = declare_var loc t res in
  res

let assign_var (loc: l) (i: ident) (x: sym): unit rws =
  let@ i' = DisEnv.getLocalName i in
  (* Attempt to set local variable. If it fails, we assume
     the variable is in an outer scope.  *)
  DisEnv.modify (LocalEnv.setLocalVar loc i x) >>
  DisEnv.write [Stmt_Assign(LExpr_Var(i'), sym_expr x, loc)]

let declare_const (loc: l) (t: ty) (i: ident) (x: sym): unit rws =
  let@ i' = DisEnv.getLocalName i in
  check_var_shadowing loc i >>
  DisEnv.modify (LocalEnv.addLocalConst loc i x t) >>
  DisEnv.write [Stmt_ConstDecl(t, i', sym_expr x, loc)]

let declare_fresh_const (loc: l) (t: ty) (name: string) (x: expr): ident rws =
  let@ i = DisEnv.nextVarName name in
  let+ () = declare_const loc t i (Exp x) in
  i

let capture_expr loc (t: ty) (x: expr): sym rws =
  let@ v = declare_fresh_const loc t "Exp" x in
  let@ i = DisEnv.getLocalName v in
  let+ () = DisEnv.modify (LocalEnv.setLocalVar loc v (Val (VUninitialized t))) in
  Exp (Expr_Var i)

(** Monadic Utilities *)

(** Symbolic implementation of an if statement that returns an expression
 *)
let sym_if (loc: l) (t: ty) (test: sym rws) (tcase: sym rws) (fcase: sym rws): sym rws =
  let@ r = test in
  (match r with
  | Val (VBool (true))  -> tcase
  | Val (VBool (false)) -> fcase
  | Val _ -> failwith ("Split on non-boolean value")
  | Exp e ->
      let@ tmp = declare_fresh_named_var loc "If" t in
      let@ i' = DisEnv.getLocalName tmp in
      (* Evaluate true branch statements. *)
      let@ (tenv,tstmts) = DisEnv.locally_
          (tcase >>= assign_var loc tmp) in
      (* Propagate incremented counter to env'. *)
      let@ env' = DisEnv.gets (fun env -> LocalEnv.sequence_merge env tenv) in
      (* Execute false branch statements with env'. *)
      let@ (fenv,fstmts) = DisEnv.locally_
          (DisEnv.put env' >> fcase >>= assign_var loc tmp) in
      let@ () = DisEnv.put (LocalEnv.join_merge tenv fenv) in
      let+ () = DisEnv.write [Stmt_If(e, tstmts, [], fstmts, loc)] in
      Exp (Expr_Var (i')))

(** Symbolic implementation of an if statement with no return *)
let unit_if (loc: l) (test: sym rws) (tcase: unit rws) (fcase: unit rws): unit rws =
  let@ r = test in
  (match r with
  | Val (VBool (true))  -> tcase
  | Val (VBool (false)) -> fcase
  | Val _ -> failwith ("Split on non-boolean value")
  | Exp e ->
      let@ (tenv,tstmts) = DisEnv.locally_ tcase in

      let@ env' = DisEnv.gets (fun env -> LocalEnv.sequence_merge env tenv) in
      let@ (fenv,fstmts) = DisEnv.locally_ (DisEnv.put env' >> fcase) in

      let@ () = DisEnv.put (LocalEnv.join_merge tenv fenv) in
      DisEnv.write [Stmt_If(e, tstmts, [], fstmts, loc)])

(** Symbolic implementation of List.for_all2 *)
let rec sym_for_all2 p l1 l2 =
  match (l1, l2) with
  | ([], []) -> DisEnv.pure sym_true
  | (a1::l1, a2::l2) -> sym_if Unknown (type_bool) (p a1 a2) (sym_for_all2 p l1 l2) (DisEnv.pure sym_false)
  | (_, _) -> invalid_arg "sym_for_all2"

(** Symbolic implementation of List.exists *)
let rec sym_exists p = function
  | [] -> DisEnv.pure sym_false
  | a::l -> sym_if Unknown (type_bool) (p a) (DisEnv.pure sym_true) (sym_exists p l)

let width_of_type (loc: l) (t: ty): int =
  match t with
  | Type_Bits (Expr_LitInt wd) -> int_of_string wd
  | _ -> unsupported loc @@ "Can't get bit width of type: " ^ pp_type t

let width_of_field (loc: l) (t: ty) (f: ident): int =
  let env = Tcheck.env0 in
  let ft =
    (match Tcheck.typeFields env loc t with
    | FT_Record rfs -> Tcheck.get_recordfield loc rfs f
    | FT_Register rfs -> let (_,t) = Tcheck.get_regfield loc rfs f in t)
  in
  width_of_type loc ft

(** Disassembly Functions *)

(** Determine the type of memory access expression (Var, Array, Field) *)
let rec type_of_load (loc: l) (x: expr): ty rws =
  let env = Tcheck.env0 in
  (match x with
  | Expr_Var(id) ->
      let@ local = DisEnv.gets (LocalEnv.getLocalVar loc id) in
      (match local with
      | Some (t,_) -> DisEnv.pure t
      | _ ->
          (match Tcheck.GlobalEnv.getGlobalVar env id with
          | Some t -> dis_type loc t (* visit types to resolve global constants. *)
          | None -> raise (EvalError (loc, "Unknown type for variable: " ^ pprint_ident id))))
  | Expr_Field(e,f) ->
      let@ t = type_of_load loc e in
      (match Tcheck.typeFields env loc t with
      | FT_Record rfs -> dis_type loc @@ Tcheck.get_recordfield loc rfs f
      | FT_Register rfs ->
        let (_,t) = Tcheck.get_regfield loc rfs f in
        dis_type loc t)
  | Expr_Array(a,i) ->
      let@ t = type_of_load loc a in
      (match Tcheck.derefType env t with
      | Type_Array(ixty, elty) -> dis_type loc elty
      | _ -> raise (EvalError (loc, "Can't type expression: " ^ pp_expr a)))
  | _ -> raise (EvalError (loc, "Can't type expression: " ^ pp_expr x)))

(** Disassemble type *)
and dis_type (loc: l) (t: ty): ty rws =
    match t with
    | Type_Bits ex ->
        let+ ex' = dis_expr loc ex in
        (Type_Bits (sym_expr ex'))
    | Type_OfExpr ex ->
        let+ ex' = dis_expr loc ex in
        (Type_OfExpr (sym_expr ex'))
    | Type_Tuple tys ->
        let+ exprs = DisEnv.traverse (dis_type loc) tys in
        (Type_Tuple exprs)
    | t' -> DisEnv.pure t'

(** Disassemble list of expressions *)
and dis_exprs (loc: l) (xs: AST.expr list): sym list rws =
    DisEnv.traverse (dis_expr loc) xs

(** Disassemble a pattern match, mirrors eval_pattern *)
and dis_pattern (loc: l) (v: sym) (x: AST.pattern): sym rws =
    (match x with
    | Pat_LitInt(l)  -> DisEnv.pure (sym_eq_int  loc v (Val (from_intLit l)))
    | Pat_LitHex(l)  -> DisEnv.pure (sym_eq_int  loc v (Val (from_hexLit l)))
    | Pat_LitBits(l) -> DisEnv.pure (sym_eq_bits loc v (Val (from_bitsLit l)))
    | Pat_LitMask(l) -> DisEnv.pure (sym_inmask  loc v (Val (from_maskLit l)))
    | Pat_Const(c)   -> let+ c' = getGlobalConst c in sym_eq loc v c'
    | Pat_Wildcard   -> DisEnv.pure sym_true
    | Pat_Tuple(ps) ->
            let vs = sym_of_tuple loc v in
            assert (List.length vs = List.length ps);
            sym_for_all2 (dis_pattern loc) vs ps
    | Pat_Set(ps) ->
            sym_exists (dis_pattern loc v) ps
    | Pat_Single(e) ->
            let+ v' = dis_expr loc e in
            sym_eq loc v v'
    | Pat_Range(lo, hi) ->
            let+ lo' = dis_expr loc lo
            and+ hi' = dis_expr loc hi in
            sym_and_bool loc (sym_le_int loc lo' v) (sym_le_int loc v hi')
    )

(** Disassemble bitslice bounds, mirrors eval_slice *)
and dis_slice (loc: l) (x: slice): (sym * sym) rws =
    (match x with
    | Slice_Single(i) ->
            let+ i' = dis_expr loc i in
            (i', Val (VInt Z.one))
    | Slice_HiLo(hi, lo) ->
            let+ hi' = dis_expr loc hi
            and+ lo' = dis_expr loc lo in
            let wd' = sym_add_int loc (sym_sub_int loc hi' lo') (Val (VInt Z.one)) in
            (lo', wd')
    | Slice_LoWd(lo, wd) ->
            let+ lo' = dis_expr loc lo
            and+ wd' = dis_expr loc wd in
            (lo', wd')
    )

(**
  Disassemble a variable load, either accessing local or global state, potentially with a chain of
  aggregate structure accesses.

  Locals are expected to all be defined by the symbolic state and assumed to never contain symbolic aggregate structures.
  These structures are not supported by the target and must be removed for successful translation.
  TODO: This does not appear to be a problem at the moment, but requires greater testing to be sure.

  Globals are expected to be defined by the global environment as either a constant or an unknown dynamic value.
  Loads of dynamic globals, along with their access chains, are stored into temporary variables to create a pure
  symbol to refer to their current value.
  *)
and dis_load loc x =
  DisEnv.scope loc "dis_load" (pp_expr x) pp_sym (dis_load_chain loc x [])

and dis_load_chain (loc: l) (x: expr) (ref: access_chain list): sym rws =
  (match x with
  | Expr_Var(id) ->
      let@ local = DisEnv.gets (LocalEnv.getLocalVar loc id) in
      (match local with
      (* Variable is local with a unknown value, capture it in a temporary *)
      | Some (t,Val (VUninitialized _)) ->
          let@ localid = DisEnv.getLocalName id in
          let@ e' = capture_expr loc t (Expr_Var localid) in
          let+ () = DisEnv.modify (LocalEnv.setLocalVar loc id e') in
          e'
      (* Variable is local with a concrete value *)
      | Some (_,Val v) ->
          DisEnv.pure @@ Val (get_access_chain loc v ref)
      (* Variable is local with a symbolic value, should not expect a structure *)
      | Some (_,Exp e) ->
          if ref = [] then DisEnv.pure @@ Exp e
          else unsupported loc "Local variable with dynamic structure"
      | None ->
          (* Variable is global, return the accessed value or the variable if not initialised *)
          let@ v = DisEnv.reads (fun env -> Env.getVar loc env id) in
          match get_access_chain loc v ref with
          | VUninitialized _ ->
              let e = expr_access_chain (Expr_Var id) ref in
              let@ t = type_of_load loc e in
              capture_expr loc t e
          | v' -> DisEnv.pure @@ Val v')
  | Expr_Field(e,f) -> dis_load_chain loc e (Field f::ref)
  | Expr_Array(a,i) ->
      let@ i = dis_expr loc i in
      (match i with
      | Val i -> dis_load_chain loc a (Index i::ref)
      | _ -> unsupported loc "Dynamic array index")
  | x -> unsupported loc @@ "Unknown Exp chain: " ^ pp_expr x)

(** Dissassemble expression. This should never return Result VUninitialized *)
and dis_expr loc x =
  let+ r = DisEnv.scope loc "dis_expr" (pp_expr x) pp_sym (dis_expr' loc x) in
  match r with
  | Val (VUninitialized _) -> raise (EvalError (loc, "dis_expr returning VUninitialized, invalidating assumption"))
  | _ -> r

and dis_expr' (loc: l) (x: AST.expr): sym rws =
    (match x with
    | Expr_If(ty, c, t, els, e) ->
            let rec eval_if xs d : sym rws = match xs with
                | [] -> dis_expr loc d
                | AST.E_Elsif_Cond (c,b)::xs' ->
                    sym_if loc ty (dis_expr loc c) (* then *)
                      (dis_expr loc b)
                    (* else *)
                      (eval_if xs' d)
            in
            eval_if (E_Elsif_Cond(c, t)::els) e
    | Expr_Binop(a, op, b) ->
            raise (EvalError (loc, "binary operation should have been removed in expression "
                   ^ Utils.to_string (PP.pp_expr x)))
    | Expr_Field(_, _) -> dis_load loc x
    | Expr_Fields(e, fs) ->
            let+ vs = DisEnv.traverse (fun f -> dis_load loc (Expr_Field(e,f))) fs in
            sym_concat loc vs
    | Expr_Slices(e, ss) ->
            let@ e' = dis_expr loc e
            and@ ss' = DisEnv.traverse (dis_slice loc) ss in
            let vs = List.map (fun (i,w) -> sym_extract_bits loc e' i w) ss' in
            DisEnv.pure (sym_concat loc vs)
    | Expr_In(e, p) ->
            let@ e' = dis_expr loc e in
            let@ p' = dis_pattern loc e' p in
            (match p' with
            | Val v -> DisEnv.pure (Val v)
            | Exp e -> capture_expr loc type_bool e)
    | Expr_Var(_) -> dis_load loc x
    | Expr_Parens(e) ->
            dis_expr loc e
    | Expr_TApply(f, tes, es) ->
            if name_of_FIdent f = "and_bool" then begin
                (match (tes, es) with
                | ([], [x; y]) ->
                    sym_if loc (type_bool) (dis_expr loc x) (* then *)
                      (dis_expr loc y)
                    (* else *)
                      (DisEnv.pure sym_false)
                | _ ->
                    raise (EvalError (loc, "malformed and_bool expression "
                       ^ Utils.to_string (PP.pp_expr x)))
                )
            end else if name_of_FIdent f = "or_bool" then begin
                (match (tes, es) with
                | ([], [x; y]) ->
                    sym_if loc (type_bool) (dis_expr loc x) (* then *)
                      (DisEnv.pure sym_true)
                    (* else *)
                      (dis_expr loc y)
                | _ ->
                    raise (EvalError (loc, "malformed or_bool expression "
                       ^ Utils.to_string (PP.pp_expr x)))
                )
            end else if name_of_FIdent f = "implies_bool" then begin
                (match (tes, es) with
                | ([], [x; y]) ->
                    sym_if loc (type_bool) (dis_expr loc x) (* then *)
                      (dis_expr loc y)
                    (* else *)
                      (DisEnv.pure sym_true)
                | _ ->
                    raise (EvalError (loc, "malformed implies_bool expression "
                       ^ Utils.to_string (PP.pp_expr x)))
                )
            end else begin
                let@ tvs = dis_exprs loc tes in
                let@ vs  = dis_exprs loc es in
                dis_funcall loc f tvs vs
            end
    | Expr_Tuple(es) ->
            let+ es' = DisEnv.traverse (dis_expr loc) es in
            sym_tuple es'
    | Expr_Unop(op, e) ->
            raise (EvalError (loc, "unary operation should have been removed"))
    | Expr_Unknown(t) -> (* TODO: Is this enough? *)
            let+ t' = dis_type loc t in
            Exp (Expr_Unknown(t'))
    | Expr_ImpDef(t, Some(s)) ->
            DisEnv.reads (fun env -> Val (Env.getImpdef loc env s))
    | Expr_ImpDef(t, None) ->
            raise (EvalError (loc, "unnamed IMPLEMENTATION_DEFINED behavior"))
    | Expr_Array(a,i) ->   dis_load loc x
    | Expr_LitInt(i) ->    DisEnv.pure (Val (from_intLit i))
    | Expr_LitHex(i) ->    DisEnv.pure (Val (from_hexLit i))
    | Expr_LitReal(r) ->   DisEnv.pure (Val (from_realLit r))
    | Expr_LitBits(b) ->   DisEnv.pure (Val (from_bitsLit b))
    | Expr_LitMask(b) ->   DisEnv.pure (Val (from_maskLit b))
    | Expr_LitString(s) -> DisEnv.pure (Val (from_stringLit s))
    )

(** Disassemble call to function *)
and dis_funcall (loc: l) (f: ident) (tvs: sym list) (vs: sym list): sym rws =
    dis_call loc f tvs vs

(** Evaluate call to procedure *)
and dis_proccall (loc: l) (f: ident) (tvs: sym list) (vs: sym list): unit rws =
    let+ _ = dis_call loc f tvs vs in ()

(** Disassemble a function call *)
and dis_call (loc: l) (f: ident) (tes: sym list) (es: sym list): sym rws =
    DisEnv.scope loc "dis_call"
        (pp_expr (Expr_TApply (f, List.map sym_expr tes, List.map sym_expr es)))
        pp_sym
        (dis_call_intercept loc f tes es)

and dis_call_intercept (loc: l) (f: ident) (tes: sym list) (es: sym list): sym rws =
  (match (name_of_FIdent f, es) with
  | ("AArch64.BranchAddr", [e]) ->
      if !debug_level >= 2 then begin
        Printf.printf "Intercepted call to AArch64.BranchAddr: Assuming no modification to destination address\n";
      end;
      DisEnv.pure e
  | _ -> dis_call' loc f tes es)

and dis_call' (loc: l) (f: ident) (tes: sym list) (es: sym list): sym rws =
    let@ fn = DisEnv.getFun loc f in
    (match fn with
    | Some (rty, atys, targs, args, loc, b) ->
        let fname = name_of_FIdent f in

        (* Nest enviroment *)
        let@ () = DisEnv.modify LocalEnv.addLevel in

        assert (List.length targs == List.length tes);

        (* Assign targs := tes *)
        let@ () = DisEnv.sequence_ @@ List.map2 (fun arg e ->
            declare_const loc type_integer arg e
            ) targs tes in

        assert (List.length atys == List.length args);
        assert (List.length atys == List.length es);

        (* Assign args := es *)
        let@ () = DisEnv.sequence_ (Utils.map3 (fun (ty, _) arg e ->
            let@ ty' = dis_type loc ty in
            declare_const loc ty' arg e
        ) atys args es) in

        (* Create return variable (if necessary).
            This is in the inner scope to allow for type parameters. *)
        let@ rv = (match rty with
        | Some (Type_Tuple ts) ->
            let@ ts' = DisEnv.traverse (dis_type loc) ts in
            let+ names = DisEnv.traverse (declare_fresh_named_var loc fname) ts' in
            Expr_Tuple (List.map (fun n -> Expr_Var n) names)
        | Some t ->
            let@ t' = dis_type loc t in
            let+ name = declare_fresh_named_var loc fname t' in
            Expr_Var name
        | None ->
            DisEnv.pure (Expr_Unknown type_unknown)) in

        let@ env = DisEnv.get in
        let@ () = DisEnv.if_ (!debug_level >= 2)
            (DisEnv.log (LocalEnv.pp_locals env ^ "\n")) in

        (* Evaluate body with new return symbol *)
        let@ _ = DisEnv.modify (LocalEnv.addReturnSymbol rv) in
        let@ () = dis_stmts b in
        let@ () = DisEnv.modify (LocalEnv.removeReturnSymbol) in

        (* Disassemble return variable expression and propagate its symbolic value
            into the containing scope. *)
        let@ result = dis_expr loc rv in
        (* Pop enviroment. *)
        let@ () = DisEnv.modify LocalEnv.popLevel in
        DisEnv.pure result
    | None -> dis_prim f tes es
    )

and dis_prim (f: ident) (tes: sym list) (es: sym list): sym rws =
    let name = name_of_FIdent f in

    match sym_prim_simplify name tes es with
    | Some s -> DisEnv.pure s
    | None ->
        let tes_vals = List.map sym_val_or_uninit tes
        and es_vals = List.map sym_val_or_uninit es in

        match filter_uninit (eval_prim name tes_vals es_vals) with
        | Some v -> DisEnv.pure (Val v)
        | None -> let f' = Expr_TApply(f, List.map sym_expr tes, List.map sym_expr es) in
            if List.mem name Value.prims_pure
                then DisEnv.pure (Exp f')
                else capture_expr Unknown type_unknown f'

and dis_lexpr loc x r: unit rws =
    DisEnv.scope loc
        "dis_lexpr" (pp_stmt (Stmt_Assign (x, sym_expr r, Unknown)))
        Utils.pp_unit
        (dis_lexpr' loc x r)

(** Remove potential effects from an lexpr *)
and resolve_lexpr (loc: l) (x: lexpr): lexpr rws =
  (match x with
  | LExpr_Field(l,f) ->
      let+ l = resolve_lexpr loc l in
      LExpr_Field(l,f)
  | LExpr_Array(l,i) ->
      let@ e = dis_expr loc i in
      let+ l = resolve_lexpr loc l in
      (match e with
      | Val i -> LExpr_Array(l, val_expr i)
      | _ -> unsupported loc @@ "Dynamic array index in LExpr")
  | _ -> DisEnv.pure(x))

(* TODO: Missing ReadWrite LExpr, which introduces some complications for Fields case *)
and dis_lexpr_chain (loc: l) (x: lexpr) (ref: access_chain list) (r: sym): unit rws =
  (match x with
  | LExpr_Field(l, f) -> dis_lexpr_chain loc l (Field f::ref) r
  | LExpr_Array(l, i) ->
      let@ e = dis_expr loc i in
      (match e with
      | Val i -> dis_lexpr_chain loc l (Index i::ref) r
      | _ -> unsupported loc @@ "Dynamic array index in LExpr")
  | LExpr_Var(id) ->
      let@ local = DisEnv.gets (LocalEnv.getLocalVar loc id) in
      (match local with
      (* Base variable is local, can update as long as its primitive *)
      | Some (_) ->
          if ref = [] then assign_var loc id r
          else unsupported loc @@ "Local variable as base of LExpr chain"
      | None ->
          (* Base variable is global, consider where it is dynamic or statically known *)
          let@ g = DisEnv.reads (fun env -> Env.getVar loc env id) in
          match get_access_chain loc g ref with
          | VUninitialized _ -> DisEnv.write [Stmt_Assign(lexpr_access_chain x ref, sym_expr r, loc)]
          (* TODO: The following seems to imply we want to be able to modify defined global state *)
          | v' -> unsupported loc @@ "Write to assumed constant global variable: " ^ pp_lexpr x)
  | _ -> unsupported loc @@ "Unknown LExpr modify constructor: " ^ pp_lexpr x)

and dis_lexpr' (loc: l) (x: lexpr) (r: sym): unit rws =
    (match x with
    | LExpr_Wildcard ->
        DisEnv.unit
    | LExpr_Var(v) ->
        dis_lexpr_chain loc x [] r
    | LExpr_Field(_,_) ->
        dis_lexpr_chain loc x [] r
    | LExpr_Fields(l,fs) ->
        let@ l = resolve_lexpr loc l in
        let@ ty = type_of_load loc (lexpr_to_expr loc l) in
        let rec set_fields (i: int) (fs: ident list): unit rws =
            (match fs with
            | [] -> DisEnv.unit
            | (f::fs') ->
                let w = width_of_field loc ty f in
                let y = sym_slice loc r i w in
                let@ () = dis_lexpr_chain loc l [Field f] y in
                set_fields (i + w) fs'
            )
        in
        set_fields 0 fs
    | LExpr_Slices(l, ss) ->
        let e = lexpr_to_expr loc l in
        let@ ty = type_of_load loc e in
        let prev_width = (match ty with
        | Type_Bits (Expr_LitInt wd) -> int_of_string  wd
        | _ -> unsupported loc "Slice LExpr to type other than bitvector") in
        let rec eval (o: sym) (ss': AST.slice list) (prev: sym): sym rws =
            (match ss' with
            | [] -> DisEnv.pure prev
            | (s :: ss) ->
                let@ (i, w) = dis_slice loc s in
                let v       = sym_extract_bits loc r o w in
                eval (sym_add_int loc o w) ss (sym_insert_bits loc prev_width prev i w v)
            )
        in
        let@ old = dis_expr loc e in
        let@ rhs = eval (Val (VInt Z.zero)) ss old in
        dis_lexpr_chain loc l [] rhs
    | LExpr_Tuple(ls) ->
        let rs = sym_of_tuple loc r in
        assert (List.length ls = List.length rs);
        DisEnv.traverse2_ (dis_lexpr loc) ls rs
    | LExpr_Array(_,_) ->
        dis_lexpr_chain loc x [] r
    | LExpr_Write(setter, tes, es) ->
        let@ tvs = dis_exprs loc tes in
        let@ vs = dis_exprs loc es in
        dis_proccall loc setter tvs (vs @ [r])
    | _ -> unsupported loc @@ "Unknown LExpr constructor: " ^ pp_lexpr x)

(** Concatenates two lists of statements, but ensures nothing is
    added after a return statement.  *)
and stmt_append (xs: stmt list) (ys: stmt list): stmt list =
    match xs with
    | [] -> ys

    (* these interrupt control flow so we shouldn't append after them. *)
    | [Stmt_FunReturn _]
    | [Stmt_ProcReturn _]
    | [Stmt_Throw _]
    | [Stmt_Dep_Undefined _]
    | [Stmt_Undefined _] -> xs

    | x::xs -> x :: stmt_append xs ys

(** Dissassemble list of statements. *)
and dis_stmts (stmts: AST.stmt list): unit rws =
    match stmts with
    | [] -> DisEnv.unit
    | (Stmt_If(c, tstmts, elsif, fstmts, loc)::rest) ->
        (* append everything after the if statement into each of its branches. *)
        let tstmts' = stmt_append tstmts rest
        and elsif' = List.map (fun (S_Elsif_Cond(e,ss)) ->
            S_Elsif_Cond(e,stmt_append ss rest)) elsif
        and fstmts' = stmt_append fstmts rest in
        dis_stmt (Stmt_If (c, tstmts', elsif', fstmts', loc))
    | (Stmt_FunReturn _ | Stmt_ProcReturn _) as ret :: rest ->
        (match rest with
        | [] -> dis_stmt ret
        | _ -> raise (DisUnsupported (stmt_loc ret,
            "unexpected statements after return: " ^
            Utils.pp_list pp_stmt rest)))
    | (s::rest) ->
        dis_stmt s >> dis_stmts rest




(** Disassemble statement *)
and dis_stmt x = DisEnv.scope (stmt_loc x) "dis_stmt" (pp_stmt x) Utils.pp_unit (dis_stmt' x)
and dis_stmt' (x: AST.stmt): unit rws =
    (* Printf.printf "dis_stmt --s-> %s\n" (pp_stmt x); *)
    (match x with
    | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        (* If a local prefix exists, add it *)
        let@ ty' = dis_type loc ty in
        DisEnv.traverse_ (declare_var loc ty') vs
    | Stmt_VarDecl(ty, v, e, loc) ->
        (* If a local prefix exists, add it *)
        (* Add the variable *)
        let@ ty' = dis_type loc ty in
        let@ e' = dis_expr loc e in
        declare_assign_var loc ty' v e'
    | Stmt_ConstDecl(ty, v, e, loc) ->
        (* If a local prefix exists, add it *)
        let@ ty' = dis_type loc ty in
        let@ e' = dis_expr loc e in
        declare_const loc ty' v e'
    | Stmt_Assign(l, r, loc) ->
        let@ r' = dis_expr loc r in
        dis_lexpr loc l r'
    | Stmt_If(c, t, els, e, loc) ->
        let rec eval_if xs d : unit rws = match xs with
        | [] -> dis_stmts d
        | S_Elsif_Cond (c,b)::xs' ->
            unit_if loc (dis_expr loc c)
              (dis_stmts b)
            (* else *)
              (eval_if xs' d)
        in
        eval_if (S_Elsif_Cond(c, t)::els) e
    | Stmt_TCall (f, tes, es, loc) ->
        let@ tes' = dis_exprs loc tes in
        let@ es' = dis_exprs loc es in
        dis_proccall loc f tes' es'
    | Stmt_FunReturn(e, loc) ->
        let@ rv = DisEnv.gets (LocalEnv.getReturnSymbol loc) in
        let@ e' = dis_expr loc e in
        dis_lexpr loc (expr_to_lexpr rv) e'
    | Stmt_ProcReturn(loc) -> DisEnv.unit
    | Stmt_Assert(e, loc) ->
        let@ e' = dis_expr loc e in
        (match e' with
        | Val v ->
            if not (to_bool loc v) then
                raise (EvalError (loc, "assertion failure during symbolic phase"))
            else
                DisEnv.unit
        | Exp e'' ->
            DisEnv.write [Stmt_Assert(e'', loc)]
        )
    | Stmt_Case(e, alts, odefault, loc) ->
        let rec dis_alts_val (alts: alt list) (d: stmt list option) (v: value): unit rws = (
            match alts with
            | [] -> (match d with
                | None -> raise (EvalError (loc, "unmatched case"))
                | Some s -> dis_stmts s)
            | Alt_Alt(ps, oc, s) :: alts' ->
                let cond = (match oc with
                | Some c -> c
                | None -> val_expr (VBool true)) in
                (* FIXME: unnchecked use of global environment in eval_pattern and eval_expr. *)
                let@ env = DisEnv.read in
                (* FIXME: should use dis_expr to partially evaluate condition as well. *)
                if List.exists (eval_pattern loc env v) ps && to_bool loc (eval_expr loc env cond) then
                    dis_stmts s
                else
                    dis_alts_val alts' d v
        ) in
        let rec dis_alts_exp (alts: alt list) (e: expr): alt list rws = (
            match alts with
            | [] -> DisEnv.pure []
            | Alt_Alt(ps, oc, s) :: alts' ->
                let@ (_,caseenv,casestmts) = DisEnv.locally (dis_stmts s) in
                let@ (restalts,restenv,reststmts) = DisEnv.locally (dis_alts_exp alts' e) in
                let@ () = DisEnv.put (LocalEnv.join_merge caseenv restenv) in
                (* FIXME: needs to merge resulting localenv states. *)
                (* NOTE: guard condition "oc" is not visited by disassembly. *)
                DisEnv.pure (Alt_Alt(ps, oc, casestmts) :: restalts)
        ) in
        let@ e' = dis_expr loc e in
        (match e' with
        | Val v -> dis_alts_val alts odefault v
        | Exp e'' ->
            let@ alts' = dis_alts_exp alts e'' in
            (* NOTE: default case "odefault" is not visited. *)
            DisEnv.write [
                Stmt_Case(e'', alts', odefault, loc)
            ]
        )
    | Stmt_For(var, start, dir, stop, body, loc) ->
        let@ start' = dis_expr loc start in
        let@ stop' = dis_expr loc stop in

        (match (start', stop') with
        | Val startval, Val stopval ->
            let rec dis_for (i: value): unit rws =
                let c = (match dir with
                | Direction_Up -> eval_leq loc i stopval
                | Direction_Down -> eval_leq loc stopval i
                ) in
                if c then
                    let i' = (match dir with
                    | Direction_Up   -> eval_add_int loc i (VInt Z.one)
                    | Direction_Down -> eval_sub_int loc i (VInt Z.one)
                    ) in
                    let@ () = dis_stmts ([Stmt_Assign (LExpr_Var var, val_expr i, loc)] @ body)
                    in
                    dis_for i'
                else
                    DisEnv.unit
            in
            declare_var loc type_integer var >>
            dis_for startval
        | _, _ ->
            raise (DisUnsupported (loc, "for loop bounds not statically known: " ^ pp_stmt x)))
    | Stmt_Unpred _
    | Stmt_ConstrainedUnpred _
    | Stmt_ImpDef (_, _)
    | Stmt_Undefined _
    | Stmt_ExceptionTaken _
    | Stmt_Dep_Unpred _
    | Stmt_Dep_ImpDef (_, _)
    | Stmt_Dep_Undefined _
    | Stmt_See (_, _)
    | Stmt_Throw (_, _)
    | Stmt_DecodeExecute (_, _, _)
    | Stmt_While (_, _, _)
    | Stmt_Repeat (_, _, _)
    | Stmt_Try (_, _, _, _, _) ->
        (* need to defer raising so this does not throw in unreachable cases. *)
        let@ () = DisEnv.unit in
        raise (DisUnsupported (stmt_loc x, "dis_stmt: unsupported statement: " ^ pp_stmt x))
    )

let dis_encoding (x: encoding) (op: value): bool rws =
    let Encoding_Block (nm, iset, fields, opcode, guard, unpreds, b, loc) = x in
    (* todo: consider checking iset *)
    (* Printf.printf "Checking opcode match %s == %s\n" (Utils.to_string (PP.pp_opcode_value opcode)) (pp_value op); *)
    let ok = (match opcode with
    | Opcode_Bits b -> eval_eq     loc op (from_bitsLit b)
    | Opcode_Mask m -> eval_inmask loc op (from_maskLit m)
    ) in
    if ok then begin
        if !trace_instruction then Printf.printf "TRACE: instruction %s\n" (pprint_ident nm);

        let@ () = DisEnv.traverse_ (function (IField_Field (f, lo, wd)) ->
            let v = extract_bits' loc op lo wd in
            if !trace_instruction then Printf.printf "      %s = %s\n" (pprint_ident f) (pp_value v);
            declare_assign_var Unknown (val_type v) f (Val v)
        ) fields in

        let@ guard' = dis_expr loc guard in
        if to_bool loc (sym_value_unsafe guard') then begin
            List.iter (fun (i, b) ->
                if eval_eq loc (extract_bits' loc op i 1) (from_bitsLit b) then
                    raise (Throw (loc, Exc_Unpredictable))
            ) unpreds;
            (* dis_encoding: we cannot guarantee that these statements are fully evaluated. *)
            let@ () = dis_stmts b in
            DisEnv.pure true
        end else begin
            DisEnv.pure false
        end
    end else begin
        DisEnv.pure false
    end

let dis_decode_slice (loc: l) (x: decode_slice) (op: value): value rws =
    (match x with
    | DecoderSlice_Slice (lo, wd) ->
        DisEnv.pure @@ extract_bits' loc op lo wd
    | DecoderSlice_FieldName f ->
        (* assumes expression always evaluates to concrete value. *)
        let+ f' = DisEnv.getVar loc f in sym_value_unsafe f'
    | DecoderSlice_Concat fs ->
        (* assumes expression always evaluates to concrete value. *)
        let+ fs' = DisEnv.traverse (DisEnv.getVar loc) fs in
        eval_concat loc (List.map sym_value_unsafe fs')
    )

(* Duplicate of eval_decode_case modified to print rather than eval *)
let rec dis_decode_case (loc: AST.l) (x: decode_case) (op: value): unit rws =
    DisEnv.scope loc "dis_decode_case" (pp_decode_case x) Utils.pp_unit
        (dis_decode_case' loc x op)
and dis_decode_case' (loc: AST.l) (x: decode_case) (op: value): unit rws =
    (match x with
    | DecoderCase_Case (ss, alts, loc) ->
            let@ vs = DisEnv.traverse (fun s -> dis_decode_slice loc s op) ss in
            let rec dis alts =
                (match alts with
                | (alt :: alts') ->
                    let@ alt' = dis_decode_alt loc alt vs op in
                    (match alt' with
                    | true -> DisEnv.unit
                    | false -> dis alts')
                | [] ->
                        raise (DisInternalError (loc, "unmatched decode pattern"))
                )
            in
            dis alts
    )

(* Duplicate of eval_decode_alt modified to print rather than eval *)
and dis_decode_alt (loc: l) (x: decode_alt) (vs: value list) (op: value): bool rws =
    DisEnv.scope loc "dis_decode_alt" (pp_decode_alt x) string_of_bool
        (dis_decode_alt' loc x vs op)
and dis_decode_alt' (loc: AST.l) (DecoderAlt_Alt (ps, b)) (vs: value list) (op: value): bool rws =
    if List.for_all2 (eval_decode_pattern loc) ps vs then
        (match b with
        | DecoderBody_UNPRED loc -> raise (Throw (loc, Exc_Unpredictable))
        | DecoderBody_UNALLOC loc -> raise (Throw (loc, Exc_Undefined))
        | DecoderBody_NOP loc -> DisEnv.pure true
        | DecoderBody_Encoding (inst, l) ->
                let@ (enc, opost, cond, exec) = DisEnv.reads (fun env -> Env.getInstruction loc env inst) in
                let@ enc_match = dis_encoding enc op in
                if enc_match then begin
                    (* todo: should evaluate ConditionHolds to decide whether to execute body *)
                    let@ env = DisEnv.read in

                    if !debug_level >= 1 then begin
                        Printf.printf "Dissasm: %s\n" (pprint_ident inst);
                        Printf.printf "\n\nENV LOCALS: %s\n" (LocalEnv.pp_value_bindings (Env.readLocals env));
                    end;

                    let opost' = (match opost with
                    | Some post ->
                        Printf.printf "also disassembling __postdecode...\n";
                        post
                    | None -> []
                    ) in
                    let@ (lenv',stmts) = DisEnv.locally_ (dis_stmts (opost' @ exec)) in

                    if !debug_level >= 2 then begin
                        Printf.printf "-----------\n";
                        List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) stmts;
                        Printf.printf "-----------\n";
                    end;

                    let@ () = DisEnv.write stmts in
                    DisEnv.pure true
                end else begin
                    DisEnv.pure false
                end
        | DecoderBody_Decoder (fs, c, loc) ->
                let@ () = DisEnv.modify (LocalEnv.addLevel) in
                let@ () = DisEnv.traverse_ (function (IField_Field (f, lo, wd)) ->
                    let v = extract_bits' loc op lo wd in
                    declare_assign_var loc (val_type v) f (Val v)
                ) fs
                in
                let@ () = dis_decode_case loc c op in
                let@ () = DisEnv.modify (LocalEnv.popLevel) in
                DisEnv.pure true
        )
    else
        DisEnv.pure false

let rec remove_unused (globals: IdentSet.t) xs = (remove_unused' globals xs)

and remove_unused' (used: IdentSet.t) (xs: stmt list): (stmt list) =
    fst @@ List.fold_right (fun stmt (acc, used) ->
        let pass = (acc, used)
        and emit (s: stmt) =
            (s::acc, IdentSet.union used (fv_stmt s))
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
        | Stmt_Assign(LExpr_Var(v), r, loc) ->
            (* TODO: Don't pass if v is global *)
            if IdentSet.mem v used
                then emit stmt
                else pass
        | Stmt_If(c, tstmts, elsif, fstmts, loc) ->
            let tstmts' = remove_unused' used tstmts in
            let fstmts' = remove_unused' used fstmts in
            let elsif' = List.map (fun (S_Elsif_Cond (c,ss)) ->
                S_Elsif_Cond (c, remove_unused' used ss)) elsif in
            (match (tstmts',fstmts',elsif') with
            | [], [], [] -> pass
            | _, _, _ -> emit (Stmt_If(c, tstmts', elsif', fstmts', loc)))
        | x -> emit x
    ) xs ([], used)

let dis_decode_entry (env: Env.t) (decode: decode_case) (op: value): stmt list =
    let DecoderCase_Case (_,_,loc) = decode in

    let env = Env.freeze env in
    let globals = IdentSet.of_list @@ List.map fst @@ Bindings.bindings (Env.readGlobals env) in
    let lenv = LocalEnv.empty () in
    let ((),lenv',stmts) = (dis_decode_case loc decode op) env lenv in
    let stmts' = Transforms.Bits.bitvec_conversion @@ remove_unused globals @@ stmts in
    if !debug_level >= 2 then begin
        Printf.printf "===========\n";
        List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) stmts';
        Printf.printf "===========\n";
    end;
    stmts'


let retrieveDisassembly (env: Env.t) (opcode: string): stmt list =
    let decoder = Eval.Env.getDecoder env (Ident "A64") in
    let DecoderCase_Case (_,_,loc) = decoder in
    (* List.iter (fun (ident, _) -> Eval.Env.setVar Unknown env ident VUninitialized) (Bindings.bindings (Eval.Env.getGlobals env).bs); *)
    dis_decode_entry env decoder (Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) (Z.of_int (int_of_string opcode))))
