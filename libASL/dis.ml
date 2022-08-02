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

let debug_level = ref 1

(** (name, arg, location) tuple for tracing disassembly calls.
    For example: ("dis_expr", "1+1", loc).
*)
type dis_trace = (string * string * l) list

exception DisError of dis_trace * exn

let print_dis_trace (trace: dis_trace) =
    String.concat "\n" @@ List.map (fun (f, e, l) ->
        Printf.sprintf "... at %s: %s --> %s" (pp_loc l) f e)
        (trace)

let () = Printexc.register_printer
    (function
    | DisError (trace, exn) ->
        Some ("DisError: " ^ Printexc.to_string exn ^ "\n"
            ^ print_dis_trace trace)
    | _ -> None)


module LocalEnv = struct
    type t = {
        locals          : sym Bindings.t list;
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
            Bindings.fold (fun k v1 bs ->
                match Bindings.find_opt k r with
                | None -> bs
                | Some v2 ->
                    let v = if v1 = v2
                        then v1 else Val (VUninitialized (sym_type v1)) in
                    Bindings.add k v bs)
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

    let pp_sym_bindings (bss: sym Bindings.t list) =
        Utils.pp_list (pp_bindings pp_sym) bss

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

    let rec search (x: ident) (bss : sym Bindings.t list): sym Bindings.t option =
        match bss with
        | (bs :: bss') ->
            if Bindings.mem x bs then Some bs else search x bss'
        | [] -> None

    let addLocalVar (loc: l) (k: ident) (v: sym) (env: t): t =
        if !trace_write then Printf.printf "TRACE: fresh %s = %s\n" (pprint_ident k) (pp_sym v);
        match env.locals with
        | (bs :: rest) -> {env with locals = (Bindings.add k v bs :: rest)}
        | []        -> raise (EvalError (loc, "LocalEnv::addLocalVar bindings empty"))

    let addLocalConst = addLocalVar

    let getLocalVar (loc: l) (x: ident) (env: t): sym option =
        (match (search x env.locals) with
        | Some bs -> Some (Bindings.find x bs)
        | None -> None)

    let setLocalVar (loc: l) (x: ident) (v: sym) (env: t): t =
        if !trace_write then Printf.printf "TRACE: write %s = %s\n" (pprint_ident x) (pp_sym v);
        let locals' = Utils.replace_in_list (fun b ->
            if (Bindings.mem x b)
                then Some (Bindings.add x v b)
                else None
            )
            env.locals in
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
            | Some v -> pure (v)
            | None -> reads (fun env -> Val (Env.getVar loc env x))

    let getVarOpt (loc: l) (x: ident): sym option rws =
        let* v = gets (LocalEnv.getLocalVar loc x) in
        match (v) with
            | Some v -> pure (Some v)
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

    let getFun (loc: l) (x: ident): (ty option * ((ty * ident) list) * ident list * ident list * AST.l * stmt list) option rws =
        reads (catch (fun env -> Env.getFun loc env x))

    let nextVarName (prefix: string): ident rws =
        let* num = stateful LocalEnv.incNumSymbols in
        gets (LocalEnv.getLocalName (Ident (prefix ^ "_" ^ string_of_int num)))

    let indent: string rws =
        let+ i = gets (fun l -> l.indent) in
        let h = i / 2 in
        let s = String.concat "" (List.init h (fun _ -> "\u{2502} \u{250a} ")) in
        if i mod 2 == 0 then
            s ^ ""
        else
            s ^ "\u{2502} "

    let log (s: string): unit rws =
        let+ i = indent in
        let s' = Str.global_replace (Str.regexp "\n") ("\n"^i) s in
        Printf.printf "%s%s\n" i s';
        ()

    let warn s = log ("WARNING: " ^ s)

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
        | Left ((DisError _) as e, bt) -> Printexc.raise_with_backtrace e bt
        | Left (exn, bt) -> Printexc.raise_with_backtrace (DisError (trace, exn)) bt
        | Right x' -> x') in
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
    | Some _ -> DisEnv.warn ("shadowing local variable: " ^ pprint_ident i)
    | None -> DisEnv.unit)

let declare_var (loc: l) (t: ty) (i: ident): unit rws =
  check_var_shadowing loc i >>
  DisEnv.modify
    (LocalEnv.addLocalVar loc i (Val (VUninitialized t))) >>
  DisEnv.write [Stmt_VarDeclsNoInit(t, [i], loc)]

let declare_assign_var (loc: l) (t: ty) (i: ident) (x: sym): unit rws =
  check_var_shadowing loc i >>
  DisEnv.modify
    (LocalEnv.addLocalVar loc i x) >>
  DisEnv.write [Stmt_VarDecl(t, i, sym_expr x, loc)]

let declare_fresh_named_var (loc: l) (name: string)  (t: ty): ident rws =
  let@ res = DisEnv.nextVarName name in
  let+ () = declare_var loc t res in
  res

let assign_var (loc: l) (i: ident) (x: sym): unit rws =
  (* Attempt to set local variable. If it fails, we assume
     the variable is in an outer scope.  *)
  DisEnv.modify (LocalEnv.trySetLocalVar loc i x) >>
  DisEnv.write [Stmt_Assign(LExpr_Var(i), sym_expr x, loc)]

let declare_const (loc: l) (ty: ty) (i: ident) (x: sym): unit rws =
  check_var_shadowing loc i >>
  DisEnv.modify (LocalEnv.addLocalConst loc i x) >>
  DisEnv.write [Stmt_ConstDecl(ty, (i), sym_expr x, loc)]

let declare_fresh_const (loc: l) (t: ty) (name: string) (x: expr): ident rws =
  let@ i = DisEnv.nextVarName name in
  let+ () = declare_const loc t i (Exp x) in
  i

let capture_expr loc (x: expr): sym rws =
  let@ v = declare_fresh_const loc type_unknown "Exp" x in
  let+ () = DisEnv.modify (LocalEnv.setLocalVar loc v
    (Val (VUninitialized (Type_OfExpr x)))) in
  Exp (Expr_Var v)

(** Monadic Utilities *)

(** Symbolic implementation of an if statement that returns an expression
  TODO:
    - What are the implications of type_unknown?
    - Can we cache a single return variable without sacrificing the simplicity of this approach?
 *)
let sym_if (loc: l) (test: sym rws) (tcase: sym rws) (fcase: sym rws): sym rws =
  let@ r = test in
  (match r with
  | Val (VBool (true))  -> tcase
  | Val (VBool (false)) -> fcase
  | Val _ -> failwith ("Split on non-boolean value")
  | Exp e ->
      let@ tmp = declare_fresh_named_var loc "If" type_unknown in
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
      Exp (Expr_Var (tmp)))

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
  | (a1::l1, a2::l2) -> sym_if Unknown (p a1 a2) (sym_for_all2 p l1 l2) (DisEnv.pure sym_false)
  | (_, _) -> invalid_arg "sym_for_all2"

(** Symbolic implementation of List.exists *)
let rec sym_exists p = function
  | [] -> DisEnv.pure sym_false
  | a::l -> sym_if Unknown (p a) (DisEnv.pure sym_true) (sym_exists p l)

(** Disassembly Functions *)

(** Disassemble type *)
let rec dis_type (loc: l) (t: ty): ty rws =
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
            sym_bool_and loc (sym_leq loc lo' v) (sym_leq loc v hi')
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

(** Dissassemble expression. This should never return Result VUninitialized *)
and dis_expr loc x =
    DisEnv.scope loc "dis_expr" (pp_expr x) pp_sym (dis_expr' loc x)

and dis_expr' (loc: l) (x: AST.expr): sym rws =
    (match x with
    | Expr_If(c, t, els, e) ->
            let rec eval_if xs d : sym rws = match xs with
                | [] -> dis_expr loc d
                | AST.E_Elsif_Cond (c,b)::xs' ->
                    sym_if loc (dis_expr loc c)
                      (dis_expr loc b)
                    (* else *)
                      (eval_if xs' d)
            in
            eval_if (E_Elsif_Cond(c, t)::els) e
    | Expr_Binop(a, op, b) ->
            raise (EvalError (loc, "binary operation should have been removed in expression "
                   ^ Utils.to_string (PP.pp_expr x)))
    | Expr_Field(e, f) ->
            let@ e' = dis_expr loc e in
            (match e' with
            | Val v -> DisEnv.pure @@ Val (get_field loc v f)
            | Exp e -> capture_expr loc (Expr_Field(e,f)))
    | Expr_Fields(e, fs) ->
            let@ e' = dis_expr loc e in
            (match e' with
            | Val v -> DisEnv.pure @@
                Val (eval_concat loc (List.map (get_field loc v) fs))
            | Exp e -> capture_expr loc (Expr_Fields(e, fs)))
    | Expr_Slices(e, ss) ->
            let@ e' = dis_expr loc e
            and@ ss' = DisEnv.traverse (dis_slice loc) ss in
            (match (e',List.exists sym_pair_has_exp ss') with
            | (Val v, false) ->
                let vs = List.map (fun (l,w) -> extract_bits loc v (sym_val_or_uninit l) (sym_val_or_uninit w)) ss' in
                DisEnv.pure @@ Val (eval_concat loc vs)
            | _ ->
                let vs = List.map (fun (l,w) ->
                    Slice_LoWd(sym_expr l, sym_expr w)) ss' in
                DisEnv.pure @@ Exp (Expr_Slices(sym_expr e', vs)))
    | Expr_In(e, p) ->
            let@ e' = dis_expr loc e in
            let@ p' = dis_pattern loc e' p in
            (match p' with
            | Val v -> DisEnv.pure (Val v)
            | Exp e -> capture_expr loc e)
    | Expr_Var id ->
            let@ idopt = DisEnv.findVar loc id in
            (match idopt with
            | None ->
                let@ id' = (DisEnv.getLocalName id) in
                raise (EvalError (loc,
                "attempt to access undeclared variable: " ^ pp_expr x
                ^ " or local name " ^ pprint_ident id'))
            | Some id' ->
                let@ v = DisEnv.getVar loc id' in
                (match sym_initialised v with
                | Some s -> DisEnv.pure s
                | None -> capture_expr loc (Expr_Var id')))
                (* TODO: Partially defined structures? *)
    | Expr_Parens(e) ->
            dis_expr loc e
    | Expr_TApply(f, tes, es) ->
            if name_of_FIdent f = "and_bool" then begin
                (match (tes, es) with
                | ([], [x; y]) ->
                    sym_if loc (dis_expr loc x) (* then *)
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
                    sym_if loc (dis_expr loc x) (* then *)
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
                    sym_if loc (dis_expr loc x) (* then *)
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
            (match sym_collect_list es' with
            | Right vals -> Val (VTuple vals)
            | Left exps -> Exp (Expr_Tuple exps))
    | Expr_Unop(op, e) ->
            raise (EvalError (loc, "unary operation should have been removed"))
    | Expr_Unknown(t) -> (* TODO: Is this enough? *)
            let+ t' = dis_type loc t in
            Exp (Expr_Unknown(t'))
    | Expr_ImpDef(t, Some(s)) ->
            DisEnv.reads (fun env -> Val (Env.getImpdef loc env s))
    | Expr_ImpDef(t, None) ->
            raise (EvalError (loc, "unnamed IMPLEMENTATION_DEFINED behavior"))
    | Expr_Array(a, i) ->
            let@ a' = dis_expr loc a in
            let@ i' = dis_expr loc i in
            (match (a',i') with
            | Val av, Val iv ->
                (match get_array loc av iv with
                | VUninitialized _ -> capture_expr loc
                    (Expr_Array(a, val_expr iv))
                | v -> DisEnv.pure @@ Val v)
            | _ -> capture_expr loc (Expr_Array(a, to_expr i')))
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
        (dis_call' loc f tes es)

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
            let@ arg' = DisEnv.gets (LocalEnv.getLocalName arg) in
            declare_const loc type_integer arg' e
            ) targs tes in

        assert (List.length atys == List.length args);
        assert (List.length atys == List.length es);

        (* Assign args := es *)
        let@ () = DisEnv.sequence_ (Utils.map3 (fun (ty, _) arg e ->
            let@ arg' = DisEnv.gets (LocalEnv.getLocalName arg) in
            let@ ty' = dis_type loc ty in
            declare_const loc ty' arg' e
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
                else capture_expr Unknown f'

and dis_lexpr loc x r: unit rws =
    DisEnv.scope loc
        "dis_lexpr" (pp_stmt (Stmt_Assign (x, sym_expr r, Unknown)))
        Utils.pp_unit
        (dis_lexpr' loc x r)

and dis_lexpr' (loc: l) (x: AST.lexpr) (r: sym): unit rws =
    match x with
    | LExpr_Write(setter, tes, es) ->
        let@ tvs = DisEnv.traverse (dis_expr loc) tes in
        let@ vs = DisEnv.traverse (dis_expr loc) es in
        dis_proccall loc setter tvs (vs @ [r])
        (* DisEnv.write [Stmt_Assign(LExpr_Write(setter, List.map sym_expr tvs, List.map sym_expr vs), sym_expr r, loc)] *)
    | LExpr_Var(v) ->
        let@ idopt = DisEnv.findVar loc v in
        let v' = (match idopt with
        | Some v' -> v'
        | None -> raise (EvalError (loc, "dis_lexpr: attempt to assign to undeclared variable"))) in
        assign_var loc v' r
    | LExpr_Tuple ls ->
        (match r with
        | Val (VTuple vs) ->
            DisEnv.sequence_ @@
                List.map2 (fun l v -> dis_lexpr loc l (Val v)) ls vs
        | Exp (Expr_Tuple es) ->
            DisEnv.sequence_ @@
                List.map2 (fun l e -> dis_lexpr loc l (Exp e)) ls es
        | _ -> DisEnv.write [Stmt_Assign (x, sym_expr r, loc)])
    | LExpr_Wildcard -> DisEnv.unit
    | LExpr_Array (arr, ind) ->
        (* NOTE: Currently, we do not traverse the array part of an array index assignment.
           Doing so would require a read-modify-write similar to eval_lexpr_modify. *)
        let@ ind' = dis_expr loc ind in
        DisEnv.write [Stmt_Assign(LExpr_Array(arr,sym_expr ind'), sym_expr r, loc)]
    | LExpr_Fields (LExpr_Var lv, fields) ->
        let@ lv' = DisEnv.getVar loc lv in
        (match lv' with
        | Val (VRecord bs) ->
            let field_width f =
                (match val_type (Bindings.find f bs) with
                | Type_Bits (Expr_LitInt n) -> int_of_string n
                | _ -> failwith "expected Type_Bits in record field assignment")
            in let rec dis_fields fs lo: unit rws =
                (match fs with
                | [] -> DisEnv.unit
                | (f::fs') ->
                    let wd = field_width f in
                    dis_lexpr loc (LExpr_Field (LExpr_Var lv, f)) (sym_slice loc r lo wd) >>
                    dis_fields fs' (lo + wd)
                )
            in
            dis_fields (List.rev fields) 0
        | Val _ -> failwith "expected VRecord in field modification"
        | _ -> DisEnv.write [Stmt_Assign(x, sym_expr r, loc)])
    | _ ->
        DisEnv.write [Stmt_Assign(x, sym_expr r, loc)]

(** Dissassemble list of statements *)
and dis_stmts (xs: AST.stmt list): unit rws =
    DisEnv.traverse_ dis_stmt xs


(** Disassemble statement *)
and dis_stmt x = DisEnv.scope (stmt_loc x) "dis_stmt" (pp_stmt x) Utils.pp_unit (dis_stmt' x)
and dis_stmt' (x: AST.stmt): unit rws =
    (* Printf.printf "dis_stmt --s-> %s\n" (pp_stmt x); *)
    (match x with
    | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        (* If a local prefix exists, add it *)
        let@ ty' = dis_type loc ty in
        let@ vs' = DisEnv.traverse DisEnv.getLocalName vs in
        DisEnv.traverse_ (declare_var loc ty') vs'
    | Stmt_VarDecl(ty, v, e, loc) ->
        (* If a local prefix exists, add it *)
        let@ v' = DisEnv.getLocalName v in
        (* Add the variable *)
        let@ ty' = dis_type loc ty in
        let@ e' = dis_expr loc e in
        declare_assign_var loc ty' v' e'
    | Stmt_ConstDecl(ty, v, e, loc) ->
        (* If a local prefix exists, add it *)
        let@ v' = DisEnv.getLocalName v in
        let@ ty' = dis_type loc ty in
        let@ e' = dis_expr loc e in
        declare_const loc ty' v' e'
    | Stmt_Assign(l, r, loc) ->
        let@ r' = dis_expr loc r in
        dis_lexpr loc l r' (* TODO: double check that both statements are added *)
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
        (* let@ e' = dis_expr loc e in *)
        let@ rv = DisEnv.gets (LocalEnv.getReturnSymbol loc) in
        let@ e' = dis_expr loc e in
        dis_lexpr loc (expr_to_lexpr rv) e'
    | Stmt_ProcReturn(loc) -> DisEnv.unit
    | Stmt_Assert(e, loc) ->
        let@ e' = dis_expr loc e in
        (match e' with
        | Val v ->
            DisEnv.write [Stmt_Assert(val_expr v, loc)]
            (* if not (to_bool loc v) then
                raise (EvalError (loc, "assertion failure during symbolic phase"))
            else
                DisEnv.unit *)
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
    | Stmt_For(v, start, dir, stop, body, loc) ->
        let@ start' = dis_expr loc start in
        let@ stop' = dis_expr loc stop in
        let@ v' = DisEnv.getLocalName v in

        (match (start', stop') with
        | Val startval, Val stopval ->
            let rec dis_for (i: value): unit rws =
                let c = (match dir with
                | Direction_Up -> eval_leq loc i stopval
                | Direction_Down -> eval_leq loc stopval i
                ) in
                if c then
                    assign_var loc v' (Val i) >>
                    dis_stmts body >>
                    let i' = (match dir with
                    | Direction_Up   -> eval_add_int loc i (VInt Z.one)
                    | Direction_Down -> eval_sub_int loc i (VInt Z.one)
                    ) in
                    dis_for i'
                else
                    DisEnv.unit
            in
            declare_var loc type_integer v' >>
            dis_for startval
        | _, _ ->
            DisEnv.warn ("for loop bounds not statically known: " ^ pp_loc loc) >>
            (* Add local variable to internal state. emitting declaration statements are not needed because
               that will be done by the interpreter.  *)
            let@ (env',body') = DisEnv.locally_
                (DisEnv.modify
                    (LocalEnv.addLocalVar loc v' (Val (VUninitialized TC.type_integer))) >>
                dis_stmts body) in
            (* Note: Check propagation of env' from loop body to outer state. assumes one iteration. *)
            DisEnv.put env' >>
            DisEnv.write
                [Stmt_For(v', sym_expr start', dir, sym_expr stop', body', loc)])
    | x -> DisEnv.write [x]
    )

(* Duplicate of eval_decode_case modified to print rather than eval *)
let rec dis_decode_case (loc: AST.l) (env: Env.t) (x: decode_case) (op: value): stmt list =
    (match x with
    | DecoderCase_Case (ss, alts, loc) ->
            let vs = List.map (fun s -> eval_decode_slice loc env s op) ss in
            let rec dis alts =
                (match alts with
                | (alt :: alts') ->
                    (match dis_decode_alt loc env alt vs op with Some stmts -> stmts | None -> dis alts')
                | [] ->
                        raise (EvalError (loc, "unmatched decode pattern"))
                )
            in
            dis alts
    )

(* Duplicate of eval_decode_alt modified to print rather than eval *)
and dis_decode_alt (loc: AST.l) (env: Env.t) (DecoderAlt_Alt (ps, b)) (vs: value list) (op: value): stmt list option =
    if List.for_all2 (eval_decode_pattern loc) ps vs then
        (match b with
        | DecoderBody_UNPRED loc -> raise (Throw (loc, Exc_Unpredictable))
        | DecoderBody_UNALLOC loc -> raise (Throw (loc, Exc_Undefined))
        | DecoderBody_NOP loc -> raise (Throw (loc, Exc_Undefined))
        | DecoderBody_Encoding (inst, l) ->
                let (enc, opost, cond, exec) = Env.getInstruction loc env inst in
                if eval_encoding env enc op then begin
                    (* todo: should evaluate ConditionHolds to decide whether to execute body *)
                    Printf.printf "Dissasm: %s\n" (pprint_ident inst);
                    (* Uncomment this if you want to see output with no evaluation *)
                    (* List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) exec; *)
                    (* Env.removeGlobals env; *)

                    (* (Printf.printf "\n\nENV GLOBALS: %s\n" (pp_bindings pp_value (Env.readGlobals env))); *)
                    (Printf.printf "\n\nENV LOCALS: %s\n" (LocalEnv.pp_value_bindings (Env.readLocals env)));
                    (* execute disassembly inside newly created local environment. *)
                    let lenv = LocalEnv.empty () in

                    let opost' = (match opost with
                    | Some post ->
                        Printf.printf "also disassembling __postdecode...\n";
                        post
                    | None -> []
                    ) in
                    let ((),lenv',stmts) = dis_stmts (opost' @ exec) env lenv in

                    Printf.printf "-----------\n";
                    List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) stmts;
                    Printf.printf "-----------\n";
                    (* List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) (join_decls (remove_unused (copy_propagation (constant_propagation stmts)))); *)
                    (* Some stmts *)
                    Some (Transforms.Bits.bitvec_conversion @@ remove_unused @@ stmts)
                    (* Some (remove_unused @@ stmts) *)
                end else begin
                    None
                end
        | DecoderBody_Decoder (fs, c, loc) ->
                (* let env = Env.empty in  *)
                List.iter (function (IField_Field (f, lo, wd)) ->
                    Env.addLocalVar loc env f (extract_bits' loc op lo wd)
                ) fs;
                Some (dis_decode_case loc env c op)
        )
    else
      None

and remove_unused xs = (remove_unused' IdentSet.empty xs)

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
