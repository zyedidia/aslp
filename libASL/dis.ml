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

let rec expr_to_lexpr (e: expr): lexpr =
    match e with
    | Expr_Var v -> LExpr_Var v
    | Expr_Tuple es -> LExpr_Tuple (List.map expr_to_lexpr es) 
    | _ -> raise (EvalError (Unknown, "unexpected expression in expr_to_lexpr coercion: " ^ pp_expr e))


module LocalEnv = struct
    type t = {
        locals          : value Bindings.t list;
        returnSymbols   : AST.lexpr list;
        numSymbols      : int;
        localPrefixes   : string list;
        implicitLevels  : (ident * value) list list;
        indent          : int;
    }

    let empty () =
        {
            locals = [Bindings.empty];
            returnSymbols = [];
            numSymbols = 0;
            localPrefixes = [];
            implicitLevels = [];
            indent = 0;
        }

    let pp_locals (env: t): string = 
        Printf.sprintf "{ locals = [%s] }" (Utils.pp_list (pp_bindings pp_value) env.locals)

    let fromEvalEnv (env: Env.t): t =
        { (empty ()) with locals = Bindings.empty :: [] }

    let addLocalVar (loc: l) (k: ident) (v: value) (env: t): t =
        if !trace_write then Printf.printf "TRACE: fresh %s = %s\n" (pprint_ident k) (pp_value v);
        match env.locals with
        | (bs :: rest) -> {env with locals = (Bindings.add k v bs :: rest)}
        | []        -> raise (EvalError (loc, "LocalEnv::addLocalVar"))

    let addLocalConst = addLocalVar

    let getReturnSymbol (loc: l) (env: t): lexpr =
        match env.returnSymbols with
        | [] -> raise (EvalError (loc, "Return not in function"))
        | (e :: rs) -> e

    let addReturnSymbol (e: lexpr) (env: t): t =
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

    let getLocalPrefix (env: t): string =
        match env.localPrefixes with
        | [] -> ""
        | x::_ -> x

    let getLocalName (x: ident) (env: t): ident =
        Ident (getLocalPrefix env ^ pprint_ident x)

    let addLocalPrefix (s: string) (env: t): t =
        {env with localPrefixes = s :: env.localPrefixes}

    let removeLocalPrefix (env: t): t =
        (match env.localPrefixes with
        | [] -> env
        | (s::ss) -> {env with localPrefixes = ss}
        )

    let addImplicitValue (arg: ident) (v: value) (env: t): t =
        match env.implicitLevels with
        | [] -> raise (EvalError (Unknown, "No levels exist"))
        | (level::levels) -> 
            {env with implicitLevels = ((arg, v)::level)::levels}

    let addImplicitLevel (env: t): t =
        {env with implicitLevels = []::env.implicitLevels}

    let popImplicitLevel (env: t): (ident * value) list * t =
        match env.implicitLevels with
        | [] -> raise (EvalError (Unknown, "No levels exist"))
        | (level::levels) -> (level, {env with implicitLevels = levels})

    let rec search (x: ident) (bss : value Bindings.t list): value Bindings.t option =
        match bss with
        | (bs :: bss') ->
            if Bindings.mem x bs then Some bs else search x bss'
        | [] -> None

    let setVar (loc: l) (x: ident) (v: value) (env: t): t =
        if !trace_write then Printf.printf "TRACE: write %s = %s\n" (pprint_ident x) (pp_value v);
        (match search x env.locals with
        (* FIXME: this incorrectly adds the variable into the most local scope.
           it should find the appropriate scope and change it. that said, maybe
           it is better to rework this to be a single "stack frame" instead of
           a list of scopes. *)
        | Some bs -> addLocalVar loc x v env
        | None    -> raise (EvalError (loc, "LocalEnv::setVar " ^ pprint_ident x ^ " " ^ pp_locals env))
        )
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

    let getVar (loc: l) (x: ident): value rws = 
        let* v = gets (fun lenv -> LocalEnv.search x lenv.locals) in
        match (v) with
            | Some b -> pure (Bindings.find x b)
            | None -> reads (fun env -> Env.getVar loc env x)

    let getVarOpt (loc: l) (x: ident): value option rws = 
        let* v = gets (fun lenv -> LocalEnv.search x lenv.locals) in
        match (v) with
            | Some b -> pure (Some (Bindings.find x b))
            | None -> reads (catch (fun env -> Env.getVar loc env x))

    let getLocalName (x: ident): ident rws = 
        let+ prefix = gets LocalEnv.getLocalPrefix in
        Ident (prefix ^ pprint_ident x)

    let findVar (loc: l) (id: ident): ident option rws =
        let* localid = gets (LocalEnv.getLocalName id) in
        let* v = getVarOpt loc localid in
        match v with
        | Some v -> pure (Some localid)
        | None -> 
            let+ v = getVarOpt loc id in 
            (match val_opt_initialised v with
            | Some v -> Some id
            | None -> None)

    let getFun (loc: l) (x: ident): (ty option * ((ty * ident) list) * ident list * ident list * AST.l * stmt list) option rws =
        reads (catch (fun env -> Env.getFun loc env x))

    let nextVarName (prefix: string): ident rws =
        let+ num = stateful LocalEnv.incNumSymbols in
        Ident (prefix ^ "Var" ^ string_of_int num)

    let indent: string rws = 
        let+ i = gets (fun l -> l.indent) in
        let h = i / 2 in
        let s = String.concat "" (List.init h (fun _ -> "\u{2502} \u{250a} ")) in
        if i mod 2 == 0 then
            s
        else 
            s ^ "\u{2502} "

    let scope (name: string) (arg: string) (pp: 'a -> string) (x: 'a rws): 'a rws = 
        let* i = indent in
        Printf.printf "%s\u{256d}\u{2500} %s --> %s\n" i name arg;
        let* () = modify (fun l -> {l with indent = l.indent + 1}) in
        let* (x,s',w') = locally x in
        (* let* i' = indent in
        List.iter (fun s -> Printf.printf "%s %s\n" i' (pp_stmt s)) w'; *)
        let+ () = write w'
        and+ () = put s'
        and+ () = modify (fun l -> {l with indent = l.indent - 1}) in
        Printf.printf "%s\u{2570}\u{2500} = %s\n" i (pp x);
        x

end

type 'a rws = 'a DisEnv.rws

let (let@) = DisEnv.Let.(let*)
let (and@) = DisEnv.Let.(and*)
let (let+) = DisEnv.Let.(let+)
let (and+) = DisEnv.Let.(and+)


let mergeEnv (xLocals: scope list) (yLocals: scope list): scope list =
    if List.length xLocals <> List.length yLocals then raise (EvalError (Unknown, "Scope lengths should be the same"));
    if List.length xLocals = 0 then [] else
    (* New list of scopes with merged locals *)
    (List.map2 (fun xScope yScope -> 
        let bs =
        (let xbs = Bindings.bindings xScope.bs in
        List.fold_left (fun bs (xKey, xV) ->
            let ybs = yScope.bs in
            if Bindings.find xKey ybs <> xV then
                Bindings.update xKey (fun xo -> match xo with Some x -> Some VUninitialized | None -> raise (EvalError (Unknown, "Unreachable"))) bs
            else
                Bindings.add xKey xV bs
        ) Bindings.empty xbs) in
        { bs }
    ) xLocals yLocals)
        

(** Convert value to a simple expression containing that value, so we can
    print it or use it symbolically *)
let to_expr = sym_expr

let is_val (x: AST.expr): bool =
    (match x with 
    | Expr_LitInt _ -> true
    | Expr_LitReal _ -> true
    | Expr_LitString _ -> true
    | Expr_Tuple _ -> true
    | x -> false
    )

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

(** Dissassemble type *)
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

(** Dissassemble list of expressions *)
and dis_exprs (loc: l) (xs: AST.expr list): sym list rws =
    DisEnv.traverse (dis_expr loc) xs

(** Evaluate bitslice bounds *)
and dis_slice (loc: l) (x: AST.slice): (sym * sym) rws =
    (match x with
    | Slice_Single(i) ->
            let+ i' = dis_expr loc i in
            (i', Val (VInt Z.one))
    | Slice_HiLo(hi, lo) ->
            let+ hi' = dis_expr loc hi
            and+ lo' = dis_expr loc lo in
            (lo', sym_add_int loc (sym_sub_int loc hi' lo') (Val (VInt Z.one)))
    | Slice_LoWd(lo, wd) ->
            let+ lo' = dis_expr loc lo
            and+ wd' = dis_expr loc wd in
            (lo', wd')
    )

(** Dissassemble a function call *)
and dis_fun (loc: l) (f: ident) (tes: AST.expr list) (es: AST.expr list): sym rws = 
    DisEnv.scope "dis_fun" 
        (pp_expr (Expr_TApply (f, tes, es)))
        pp_sym 
        (dis_fun' loc f tes es)

and dis_fun' (loc: l) (f: ident) (tes: AST.expr list) (es: AST.expr list): sym rws =
    if name_of_FIdent f = "and_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> 
            let@ x' = dis_expr loc x in
            (match x' with
            | Val v -> 
                if to_bool loc v then dis_expr loc y else DisEnv.pure sym_false
            | Exp e -> 
                let+ y' = dis_expr loc y in
                (Exp (Expr_TApply(f, tes, [e; to_expr y'])))
            )
        | _ ->
            raise (EvalError (loc, "malformed and_bool expression"))
        )
    end else if name_of_FIdent f = "or_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> 
            let@ x' = dis_expr loc x in
            (match x' with
            | Val v -> 
                if to_bool loc v then DisEnv.pure sym_true else dis_expr loc y
            | Exp e -> 
                let+ y' = dis_expr loc y in
                (Exp (Expr_TApply(f, tes, [e; to_expr y'])))
            )
        | _ ->
            raise (EvalError (loc, "malformed or_bool expression"))
        )
    end else if name_of_FIdent f = "implies_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> 
            let@ x' = dis_expr loc x in
            (match x' with
            | Val v -> 
                if to_bool loc v then dis_expr loc y else DisEnv.pure sym_true
            | Exp e -> 
                let+ y' = dis_expr loc y in
                (Exp (Expr_TApply(f, tes, [e; to_expr y'])))
            )
        | _ ->
            raise (EvalError (loc, "malformed implies_bool expression"))
        )
    end else 
        let@ fn = DisEnv.getFun loc f in
        (match fn with
        | Some (rty, atys, targs, args, loc, b) ->

            (* Add return type variables *)
            (* For each of these, if one already exists, add it to a stack to be restored after *)
            let@ () = DisEnv.modify LocalEnv.addImplicitLevel in
            let@ () = DisEnv.sequence_ @@ List.map2 (fun arg e -> 
                let@ v' = DisEnv.getVarOpt loc arg in
                let@ () = (match v' with
                    | Some v' -> DisEnv.modify (LocalEnv.addImplicitValue arg v')
                    | None -> DisEnv.unit) in
                let@ e' = dis_expr loc e in
                (match e' with
                    | Val v -> DisEnv.modify (LocalEnv.addLocalVar loc arg v)
                    | Exp _ -> raise (Invalid_argument "asdf"))
            ) targs tes in
            (* let _ =
            write_multi (List.concat (List.map2 (fun arg e -> 
                read (
                    let* e' = dis_expr loc env e in
                    (try (Env.addImplicitValue env arg (Env.getVar loc env arg)) with EvalError _ -> ());
                    (match e' with
                    | Val v -> return (Env.addLocalVar loc env arg v)
                    | Exp _ -> return ()
                    )
                ) 
            ) targs tes)) in *)

            (* Add return variable declarations *)
            let fName = name_of_FIdent f in
            let make_return_vars (i: int) (t: ty): ident rws =
                let name = Ident (Printf.sprintf "%s_return%d" fName i) in
                let+ () = dis_stmt (Stmt_VarDeclsNoInit(t, [name], Unknown)) in
                name in 
            let@ rv = (match rty with
                | Some (Type_Tuple ts) -> 
                    let+ names = DisEnv.traverse make_return_vars ts in
                    Expr_Tuple (List.mapi (fun i n -> Expr_Var n) names)
                | Some t -> 
                    let+ name = make_return_vars t in
                    Expr_Var name
                | None -> 
                    raise (EvalError (loc, "Unexpected function return type"))) in            

            let@ () = DisEnv.modify (LocalEnv.addReturnSymbol (expr_to_lexpr rv)) in
            (* Get the return symbol we just made so we can return it later *)
            (* let@ rv = DisEnv.gets (LocalEnv.getReturnSymbol loc) in *)

            (* Add local parameters, avoiding name collisions *)
            (* Also print what the parameter refers to *)
            let@ num = DisEnv.stateful LocalEnv.incNumSymbols in
            let localPrefix = fName ^ string_of_int num in
            let@ () = DisEnv.sequence_ (Utils.map3 (fun (ty, _) arg ex -> 
                let@ ex' = dis_expr loc ex in 
                let v = (match ex' with 
                    | Val v -> v
                    | Exp _ -> VUninitialized) in
                let name = (Ident (localPrefix ^ pprint_ident arg)) in
                let@ () = DisEnv.modify (LocalEnv.addLocalVar loc  name v) in
                let@ ty' = dis_type loc ty in
                DisEnv.write [Stmt_VarDecl(ty', name, to_expr ex', loc)]
                
            ) atys args es) in

            (* print out the body *)
            let@ () = DisEnv.modify (LocalEnv.addLocalPrefix localPrefix)
            and@ () = dis_stmts b
            and@ () = DisEnv.modify (LocalEnv.removeReturnSymbol)
            and@ () = DisEnv.modify (LocalEnv.removeLocalPrefix) in

            (* Restore implicit values *)
            let@ level = DisEnv.stateful LocalEnv.popImplicitLevel in
            let@ () = DisEnv.traverse_ (fun (arg, v) -> 
                DisEnv.modify (LocalEnv.addLocalVar loc arg v)) level in

            (* Return the return symbol to be used in expressions e.g. assignments *)
            DisEnv.pure (Exp rv)
        | None ->
            let+ tes' = dis_exprs loc tes
            and+ es' = dis_exprs loc es in

            let tes_vals = (List.map sym_val_or_uninit tes')
            and es_vals = (List.map sym_val_or_uninit es') in

            (* unwrap evaluated value only if it is not uninitialised *)
            match val_opt_initialised (eval_prim (name_of_FIdent f) tes_vals es_vals) with
            | Some v -> Val v
            | None -> Exp (Expr_TApply(f, List.map to_expr tes', List.map to_expr es'))
        )

(** Dissassemble expression. This should never return Result VUninitialized *)
and dis_expr loc x = 
    DisEnv.scope "dis_expr" (pp_expr x) pp_sym (dis_expr' loc x)
and dis_expr' (loc: l) (x: AST.expr): sym rws =
    match x with
    | Expr_If(c, t, els, e) ->
        let rec eval_if xs e: sym rws = match xs with
            | [] -> dis_expr loc e
            (* If we cannot evaluate the condition, print the whole statement *)
            | AST.E_Elsif_Cond (cond, branch)::xs' -> 
                let@ cond' = dis_expr loc cond in
                match cond' with
                | Val v ->
                    if to_bool loc v then
                        (* Just print this branch *)
                        dis_expr loc branch
                    else
                        (* Print whatever the rest of the branches turn out to be *)
                        eval_if xs' e
                | Exp cond'' -> 
                    (* FIXME: in if expression case, we need to generate and if _statement_ which
                       assigns to a new variable representing the if result. *)

                    (* let tEnv = Env.copy env in
                    let elsEnv = List.map (fun _ -> Env.copy env) (Utils.range 0 (List.length els)) in
                    let eEnv = Env.copy env in *)
                    let@ (b',benv,bstmts) = DisEnv.locally (dis_expr loc branch) in
                    (* FIXME: the branch's statements should only be executed when cond evaluates to true. *)
                    let@ () = DisEnv.write bstmts in 
                    let@ (xs',xsenv,xsstmts) = DisEnv.locally (eval_if xs' e) in
                    let@ () = DisEnv.write xsstmts in 
                    

                    (* let* b' = Env.nest (fun env' -> dis_expr loc env' branch) tEnv in
                    let* els' = dis_if_expr_no_remove loc elsEnv els in
                    let* e' = Env.nest (fun env' -> dis_expr loc env' e) eEnv in *)
                    (* Env.setLocals env (List.fold_left mergeEnv (Env.getLocals tEnv) ((Env.getLocals eEnv)::(List.map Env.getLocals elsEnv))); *)
                    DisEnv.pure (Exp (Expr_If(
                        cond'', 
                        to_expr b',
                        [],
                        to_expr xs'
                    )))
        in
        eval_if (E_Elsif_Cond(c, t)::els) e
    (* NOTE: This does not consider early returns currently. It also doesn't handle recursive calls *)
    | Expr_TApply(f, tes, es) ->
        dis_fun loc f tes es
    | Expr_Var id ->
        let@ idopt = DisEnv.findVar loc id in
        (match idopt with
        (* variable not found *)
        | None -> DisEnv.pure (Exp (Expr_Var id))
        | Some id' -> 
            let+ v = DisEnv.getVar loc id' in
            (match v with
            | VUninitialized -> (Exp (Expr_Var id'))
            | v -> (Val v)))
    | Expr_In(e, p) ->
        let@ e' = dis_expr loc e in
        (match e' with
        | Val v -> 
            let+ v' = DisEnv.reads (fun env -> eval_pattern loc env v p) in 
            (Val (from_bool v'))
        | Exp _ -> DisEnv.pure (Exp x))
    | Expr_Slices(e, ss) ->
        let@ ss' = DisEnv.traverse (dis_slice loc) ss in
        let@ e' = dis_expr loc e in
        (match e' with
        | Val v ->
            if List.exists sym_pair_has_exp ss' then
                DisEnv.pure (Exp (Expr_Slices(to_expr (Val v), 
                    List.map (fun (i, w) -> Slice_LoWd(to_expr i, to_expr w)) ss')))
            else
                let vs = List.map (fun s -> 
                    (match s with
                    | (Val v1, Val v2) -> extract_bits loc v v1 v2
                    (* should never reach this _ case due to List.exists check above. *)
                    | _ -> raise (EvalError (loc, "Unreachable: Shouldn't have expression in bit slice\n")))
                    ) ss' in
                DisEnv.pure (Val (eval_concat loc vs))
        | Exp e' -> 
            DisEnv.pure (Exp (Expr_Slices(e', List.map (fun (l,w) ->
                Slice_LoWd(to_expr l, to_expr w)
            ) ss'))))
    | Expr_Tuple(es) ->
        let+ es' = DisEnv.traverse (dis_expr loc) es in
        (match sym_collect_list es' with
        | Right vals -> Val (VTuple vals)
        | Left exps -> Exp (Expr_Tuple exps))
    | Expr_Array(a, i) ->
        let@ a' = dis_expr loc a in
        let+ i' = dis_expr loc i in
        (match (a',i') with
        | Val av, Val iv -> 
            (match get_array loc av iv with
            | VUninitialized -> Exp (Expr_Array(a, val_expr iv))
            | v -> Val v)
        | _ -> Exp (Expr_Array(a, to_expr i')))
    | x -> 
        (* FIXME: dangerous use of eval_expr in default case of dis_expr *)
        let+ x' = DisEnv.reads (DisEnv.catch (fun env -> (eval_expr loc env x))) in
        match val_opt_initialised x' with
        | None -> Exp x
        | Some v -> Val v

(** Evaluate and simplify guards and bodies of an elseif chain, without removing branches *)
(* and dis_if_expr_no_remove (loc: l) (envs: Env.t list) (xs: e_elsif list): e_elsif list writer = 
    match (xs, envs) with
    | ([], []) -> return []
    | ((AST.E_Elsif_Cond (cond, b)::xs'), (env::envs')) ->
        let* cond' = dis_expr loc env cond in
        let* b' = Env.nest (fun env' -> dis_expr loc env' b) env in
        let* els' = dis_if_expr_no_remove loc envs' xs' in
        return (AST.E_Elsif_Cond(
            to_expr (cond'), 
            to_expr (b')
        ) :: (els'))
    | _ -> raise (EvalError (loc, "Env and e_elsif list must have the same length")) *)

and dis_lexpr (loc: l) (x: AST.lexpr) (r: sym): unit rws =
    match x with
    | LExpr_Write(setter, tes, es) ->
        let@ tvs = DisEnv.traverse (dis_expr loc) tes in
        let@ vs = DisEnv.traverse (dis_expr loc) es in
        (match (sym_collect_list tvs, sym_collect_list vs, r) with
        | Right tvs', Right vs', Val r' -> 
            (* FIXME: use of eval_proccall in dis_lexpr *)
            DisEnv.reads (fun env -> 
                eval_proccall loc env setter tvs' (vs' @ [r']))
        | _, _, _ -> 
            DisEnv.write [Stmt_Assign(LExpr_Write(
                setter, List.map to_expr tvs, List.map to_expr vs), sym_expr r, loc)])
    | LExpr_Var(v) ->
        let@ idopt = DisEnv.findVar loc v in
        let v' = (match idopt with 
        | Some v' -> v' 
        | None -> v) in
        (match r with
        | Val r' -> 
            DisEnv.modify (LocalEnv.setVar loc v' r')
        | Exp e -> 
            let@ () = DisEnv.modify (LocalEnv.setVar loc v' VUninitialized) in
            DisEnv.write [Stmt_Assign(LExpr_Var(v'), e, loc)])
    | LExpr_Wildcard -> DisEnv.unit
    | LExpr_Tuple ls ->
        let rs = (match r with
        | Val (VTuple vs) -> Some (List.map (fun x -> Val x) vs)
        | Exp (Expr_Tuple es) -> Some (List.map (fun x -> Exp x) es)
        | _ -> None) in
        (match rs with
        | Some rs -> 
            DisEnv.sequence_ (List.map2 (fun l r -> dis_lexpr loc l r) ls rs)
        | None -> 
            DisEnv.write [Stmt_Assign (x, to_expr r, loc)])
    | _ -> 
        match r with
        | Val rv -> 
            (* FIXME: dangerous use of eval_lexpr in dis_lexpr fallback case. *)
            let+ opt = DisEnv.reads (DisEnv.catch 
                (fun env -> eval_lexpr loc env x rv)) in
            (match opt with
            | Some () -> ()
            | None -> raise (EvalError (loc, "error in eval_lexpr fallback of dis_lexpr")))
        | Exp re -> DisEnv.write [Stmt_Assign(x, re, loc)]

(** Dissassemble list of statements *)
and dis_stmts (xs: AST.stmt list): unit rws =
    DisEnv.traverse_ dis_stmt xs
    (* write_multi (List.concat (List.map read (List.map (dis_stmt env) xs))) *)

(** Evaluate and simplify guards and bodies of an elseif chain, without removing branches *)
(* and dis_if_stmt_no_remove (loc: l) (envs: Env.t list) (xs: s_elsif list): s_elsif list writer = 
    match (xs, envs) with
    | ([], []) -> return []
    | ((AST.S_Elsif_Cond (cond, b)::xs'), (env::envs')) ->
        let* cond' = dis_expr loc env cond in
        let* els' = dis_if_stmt_no_remove loc envs' xs' in
        let b' = Env.nest (fun env' -> dis_stmts env' b) env in
        return (AST.S_Elsif_Cond(to_expr cond', read (b'))::(els'))
    | _ -> raise (EvalError (loc, "Env list and s_elsif list must be the same length")) *)

(** Disassemble statement *)
and dis_stmt x = DisEnv.scope "dis_stmt" (pp_stmt x) Utils.pp_unit (dis_stmt' x)
and dis_stmt' (x: AST.stmt): unit rws =
    (* Printf.printf "dis_stmt --s-> %s\n" (pp_stmt x); *)
    (match x with
    | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        (* If a local prefix exists, add it *)
        let@ vs' = DisEnv.traverse DisEnv.getLocalName vs in
        (* Add the variables *)
        let@ () = DisEnv.traverse_ (fun v -> 
            DisEnv.modify (LocalEnv.addLocalVar loc v VUninitialized)) vs' in
        (* Print the declarations *)
        let@ ty' = dis_type loc ty in
        DisEnv.write [Stmt_VarDeclsNoInit(ty', vs', loc)]
    | Stmt_VarDecl(ty, v, i, loc) ->
        (* If a local prefix exists, add it *)
        let@ v' = DisEnv.getLocalName v in
        (* Add the variable *)
        let@ i' = dis_expr loc i in
        (match i' with
        | Val i' -> DisEnv.modify (LocalEnv.addLocalVar loc v' i')
        | Exp ex ->
            let@ () = DisEnv.modify (LocalEnv.addLocalVar loc v' VUninitialized) in
            let@ ty' = dis_type loc ty in
            DisEnv.write [Stmt_VarDecl(ty', v', ex, loc)]
        )
    | Stmt_ConstDecl(ty, v, i, loc) ->
        (* If a local prefix exists, add it *)
        let@ v' = DisEnv.getLocalName v in
        let@ i' = dis_expr loc i in
        (match i' with
        | Val i' -> DisEnv.modify (LocalEnv.addLocalConst loc v' i')
        | Exp ex -> 
            (* Declare constant with uninitialized value and just use symbolically *)
            let@ () = DisEnv.modify (LocalEnv.addLocalConst loc v' VUninitialized) in
            let@ ty' = dis_type loc ty in
            DisEnv.write [Stmt_ConstDecl(ty', v', ex, loc)]
        )
    | Stmt_Assign(l, r, loc) ->
        let@ r' = dis_expr loc r in
        (match (l, r') with
        | (LExpr_Tuple(les), Exp Expr_Tuple(es)) ->
            (* unpack tuple destructuring. *)
            (DisEnv.sequence_ @@ List.map2 (fun le e -> 
                dis_stmt (Stmt_Assign(le, e, loc))) les es)
        | _ ->
            dis_lexpr loc l r' (* TODO: double check that both statements are added *)
        )
    | Stmt_If(c, t, els, e, loc) ->
        let rec eval_if (xs: s_elsif list) (e: stmt list): unit rws = 
            match xs with
            | [] -> dis_stmts e
            | AST.S_Elsif_Cond (cond, b)::xs' ->
                let@ cond' = dis_expr loc cond in
                (match cond' with
                | Val v -> 
                    if to_bool loc v then
                        (* Just print this branch *)
                        dis_stmts b
                    else
                        (* Print whatever the rest of the branches turn out to be *)
                        eval_if xs' e
                (* We have to print out all branches now because we don't know
                   whether or not this one is true. We can still simplify 
                   guards and bodies though *)
                | Exp cond'' ->
                    let@ (_,benv,bstmts) = DisEnv.locally (dis_stmts b) in
                    (* let@ () = DisEnv.write bstmts in  *)
                    let@ (_,xsenv,xsstmts) = DisEnv.locally (eval_if xs' e) in
                    (* let@ () = DisEnv.write xsstmts in  *)

                    (* FIXME: check Stmt_If in dis_stmt, incorrectly assumes environment after if is not modified. *)
                    DisEnv.write [Stmt_If(
                        cond'', 
                        bstmts,
                        [],
                        xsstmts,
                        loc
                    )]
                     
                    (* let tEnv = Env.copy env in
                    let elsEnv = List.map (fun _ -> Env.copy env) (Utils.range 0 (List.length els)) in
                    let eEnv = Env.copy env in
                    let t' = read (Env.nest (fun env' -> dis_stmts env' t) tEnv) in
                    let* els' = dis_if_stmt_no_remove loc elsEnv els in
                    let e' = read (Env.nest (fun env' -> dis_stmts env' e) eEnv) in
                    Env.setLocals env (List.fold_left mergeEnv (Env.getLocals tEnv) ((Env.getLocals eEnv)::(List.map Env.getLocals elsEnv)));
                    write (Stmt_If(cond'', t', els', e', loc)) *)
                )
        in
        eval_if (S_Elsif_Cond(c, t)::els) e
    | Stmt_FunReturn(e, loc) ->
        (* FIXME: replacing return with assignment is unsound if
           return appears in the middle of a list of statements. *)
        let@ rv = DisEnv.gets (LocalEnv.getReturnSymbol loc) in
        dis_stmt (Stmt_Assign (rv, e, loc))
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
            (* let altEnvs = List.map (fun _ -> Env.copy env) (Utils.range 0 (List.length alts)) in
            let defEnv = Env.copy env in
            let result = write (Stmt_Case(
                e'', 
                List.map2 (fun (Alt_Alt(ps, oc, s)) altEnv -> Alt_Alt(ps, oc, read (Env.nest (fun env' -> (dis_stmts env' s)) altEnv))) alts altEnvs, 
                (match odefault with None -> None | Some s -> Some (read (Env.nest (fun env' -> (dis_stmts env s)) defEnv))), 
                loc
            )) in
            Env.setLocals env (List.fold_left mergeEnv (Env.getLocals defEnv) (List.map Env.getLocals altEnvs));
            result *)
        )
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
                    (match opost with
                    | Some post -> List.iter (function s -> Printf.printf "%s\n" (pp_stmt s)) post;
                        (*List.iter (eval_stmt env) post*)
                    | None -> ()
                    );
                    (* todo: should evaluate ConditionHolds to decide whether to execute body *)
                    Printf.printf "Dissasm: %s\n" (pprint_ident inst);
                    (* Uncomment this if you want to see output with no evaluation *)
                    (* List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) exec; *)
                    (* Env.removeGlobals env; *)

                    (* execute disassembly inside newly created local environment. *)
                    let lenv = LocalEnv.fromEvalEnv env in
                    let (_,lenv',stmts) = dis_stmts exec env lenv in
                    (* List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) stmts; *)
                    (* List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) (join_decls (remove_unused (copy_propagation (constant_propagation stmts)))); *)
                    Some stmts
                    (* Some (join_decls (remove_unused (copy_propagation (constant_propagation stmts)))); *)
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

and remove_unused (xs: stmt list): stmt list =
     fst @@ List.fold_right (fun stmt (acc, idents) ->
        let newIdents = IdentSet.union idents (fv_stmt stmt) in
        match stmt with
        | Stmt_VarDeclsNoInit(ty, vs, loc) -> 
            (match List.filter (fun ident -> IdentSet.mem ident idents) vs with
            | [] -> (acc, idents)
            | xs -> (stmt :: acc, idents)
            )
        | Stmt_VarDecl(ty, v, i, loc) -> if IdentSet.mem v idents then (stmt :: acc, newIdents) else (acc, idents)
        | Stmt_ConstDecl(ty, v, i, loc) -> if IdentSet.mem v idents then (stmt :: acc, newIdents) else (acc, idents)
        | Stmt_Assign(LExpr_Var(v), r, loc) -> if IdentSet.mem v idents then (stmt :: acc, newIdents) else (acc, idents)
        | x -> (x :: acc, newIdents)
    ) xs ([], IdentSet.empty)

and constant_propagation (xs: stmt list): stmt list =
    match List.fold_left (fun (acc, bs) stmt -> 
        match stmt with
        | Stmt_VarDecl(ty, v, i, loc) -> if is_val i then (acc, Bindings.add v i bs) else (acc @ [subst_stmt bs stmt], bs)
        | Stmt_ConstDecl(ty, v, i, loc) -> if is_val i then (acc, Bindings.add v i bs) else (acc @ [subst_stmt bs stmt], bs)
        | Stmt_Assign(LExpr_Var(v), r, loc) -> if is_val r then (acc, Bindings.add v r bs) else (acc @ [subst_stmt bs stmt], bs)
        | x -> (acc @ [subst_stmt bs stmt], bs)
    ) ([], Bindings.empty) xs with (acc, bs) -> acc

and copy_propagation (xs: stmt list): stmt list =
    match List.fold_left (fun (acc, bs) stmt -> 
        match stmt with
        | Stmt_VarDecl(ty, v, i, loc) -> 
            (* If we remove the statement, leave the declaration to be removed or joined later *)
            (match copy_propagation_helper v i bs acc stmt with (stmts, bs) -> if stmts = acc then (stmts @ [Stmt_VarDeclsNoInit(ty, [v], loc)], bs) else (stmts, bs))
        | Stmt_ConstDecl(ty, v, i, loc) ->
            copy_propagation_helper v i bs acc stmt
        | Stmt_Assign(LExpr_Var(v), r, loc) ->
            copy_propagation_helper v r bs acc stmt
        | x -> (acc @ [subst_stmt bs stmt], bs)
    ) ([], Bindings.empty) xs with (acc, bs) -> acc  

and copy_propagation_helper (l: ident) (r: AST.expr) (bs: expr Bindings.t) (acc: stmt list) (stmt: stmt): stmt list * expr Bindings.t =
    let newBs = if Bindings.mem l bs then Bindings.remove l bs else bs in
    (match r with 
    | Expr_Var(i) -> 
        if Bindings.mem i newBs then
            (acc, remove_reassigned l (Bindings.add l (Bindings.find i newBs) newBs))
        else
            (acc, remove_reassigned l (Bindings.add l r newBs))
    | _ -> (acc @ [subst_stmt bs stmt], newBs))

and remove_reassigned (l: ident) (bs: expr Bindings.t): expr Bindings.t =
    List.fold_left (fun bs' (ident, expr) -> if expr = Expr_Var(l) then Bindings.remove ident bs' else bs') bs (Bindings.bindings bs);

(* Don't print no init decls until they are assigned *)
and join_decls (xs: stmt list): stmt list =
    match List.fold_left (fun (acc, bs) stmt -> 
        (match stmt with
        | Stmt_VarDeclsNoInit(ty, vs, loc) ->
            (acc, List.fold_left (fun bs v -> Bindings.add v ty bs) bs vs)
        | Stmt_Assign(LExpr_Var(ident), r, loc) -> 
            if Bindings.mem ident bs then 
                (acc @ [Stmt_VarDecl(Bindings.find ident bs, ident, r, loc)], Bindings.remove ident bs)
            else
                (acc @ [stmt], bs)
        | _ -> (acc @ [stmt], bs)
        )
    ) ([], Bindings.empty) xs with (acc, bs) -> acc
