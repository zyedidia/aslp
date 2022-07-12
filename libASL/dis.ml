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


type 'a writer = 'a * stmt list

let return x = (x, [])

let write s = ((),[s])

let write_multi ss = ((), ss)

let ( let* ) (x: 'a writer) (f: 'a -> 'b writer): 'b writer =
    let (ex, stmts1) = x in
    let (ex', stmts2) = f ex in
    (ex', stmts1 @ stmts2)

let (let**) (xs: ('a writer) list) (f: 'a list -> 'b writer): 'b writer =
    let xsa = List.map (fun (a, _) -> a) xs in
    let xsbs = List.concat (List.map (fun (_, bs) -> bs) xs) in
    let (ex', stmts) = f xsa in
    (ex', xsbs @ stmts)

let read (w: 'a writer): stmt list =
    match w with (_, stmts) -> stmts

let pp_result_or_simplified (rs: sym): string = 
    match rs with
    | Val v -> pp_value v
    | Exp e -> pp_expr e


module LocalEnv = struct
    type t = {
        locals          : value Bindings.t list;
        returnSymbols   : AST.expr list;
        numSymbols      : int;
        localPrefixes   : string list;
        implicitLevels  : (ident * value) list list
    }

    let empty () =
        {
            locals = [Bindings.empty];
            returnSymbols = [];
            numSymbols = 0;
            localPrefixes = [];
            implicitLevels = [];
        }

    let fromEvalEnv (env: Env.t): t =
        { (empty ()) with locals = Bindings.empty :: Env.readLocals env }

    let addLocalVar (loc: l) (k: ident) (v: value) (env: t): t =
        if !trace_write then Printf.printf "TRACE: fresh %s = %s\n" (pprint_ident k) (pp_value v);
        match env.locals with
        | (bs :: rest) -> {env with locals = (Bindings.add k v bs :: rest)}
        | []        -> raise (EvalError (loc, "LocalEnv::addLocalVar"))

    let getReturnSymbol (loc: l) (env: t): AST.expr =
        match env.returnSymbols with
        | [] -> raise (EvalError (loc, "Return not in function"))
        | (e :: rs) -> e

    let addReturnSymbol (e: AST.expr) (env: t): t =
        {env with returnSymbols = e :: env.returnSymbols}

    let removeReturnSymbol (env: t): t =
        match env.returnSymbols with
        | [] -> env
        | (s::ss) -> {env with returnSymbols = ss} 

    let getNumSymbols (env: t): int =
        env.numSymbols

    let incNumSymbols (env: t): t =
        {env with numSymbols = env.numSymbols + 1}

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
end

module DisEnv = struct
    include Rws.Make(struct
        type r = Env.t
        type w = stmt list
        type s = LocalEnv.t
        let mempty = []
        let mappend = (@)
    end)

    let incNumSymbols: int rws =
        modify LocalEnv.incNumSymbols 
        >>= fun _ -> gets LocalEnv.getNumSymbols


    let getVar (loc: l) (x: ident): value rws = 
        fun env lenv -> 
        let v = match (LocalEnv.search x lenv.locals) with
            | Some b -> Bindings.find x b
            | None -> Env.getVar loc env x
        in (v, lenv, mempty)

    let getFun (loc: l) (x: ident): (ty option * ((ty * ident) list) * ident list * ident list * AST.l * stmt list) option rws =
        reads (fun env ->
            try Some (Env.getFun loc env x)
            with EvalError _ -> None)
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
let rec to_expr (v: sym): AST.expr = 
    match v with
    | Val x ->
        (match x with 
        | VBool b -> Expr_LitInt(if b then "1" else "0")
        | VEnum (id, n) -> Expr_LitInt(string_of_int n)
        | VInt n -> Expr_LitInt(Z.to_string n)
        | VReal n -> Expr_LitReal(Q.to_string n)
        | VBits {n; v} -> Expr_LitInt(Z.to_string v)
        | VString s -> Expr_LitString(s)
        | VTuple vs -> Expr_Tuple(List.map (fun v -> to_expr (Val v)) vs)
        (* TODO: definitely get rid of this. Should convert every case *)
        | x -> raise (EvalError (Unknown, "Casting unhandled value type to expression"))
        )
    | Exp x -> x

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

let dis_expr' (loc: l) (ex: expr): sym rws = assert false

(** Dissassemble type *)
let rec dis_type (loc: l) (t: ty): ty rws =
    match t with
    | Type_Bits ex ->
        let+ ex' = dis_expr' loc ex in
        (Type_Bits (sym_expr ex'))
    | Type_OfExpr ex ->
        let+ ex' = dis_expr' loc ex in
        (Type_OfExpr (sym_expr ex'))
    | Type_Tuple tys ->
        let+ exprs = DisEnv.traverse (dis_type loc) tys in
        (Type_Tuple exprs)
    | t' -> DisEnv.pure t'

(** Dissassemble list of expressions *)
and dis_exprs (loc: l) (xs: AST.expr list): sym list rws =
    assert false;
    (* DisEnv.traverse (dis_expr loc) xs *)

(** Evaluate bitslice bounds *)
and dis_slice (loc: l) (x: AST.slice): (sym * sym) rws =
    (match x with
    | Slice_Single(i) ->
            let+ i' = dis_expr' loc i in
            (i', Val (VInt Z.one))
    | Slice_HiLo(hi, lo) ->
            let+ hi' = dis_expr' loc hi
            and+ lo' = dis_expr' loc lo in
            (lo', sym_add_int loc (sym_sub_int loc hi' lo') (Val (VInt Z.one)))
    | Slice_LoWd(lo, wd) ->
            let+ lo' = dis_expr' loc lo
            and+ wd' = dis_expr' loc wd in
            (lo', wd')
    )

(** Dissassemble a function call *)
and dis_fun (loc: l) (f: ident) (tes: AST.expr list) (es: AST.expr list): sym rws =
    if name_of_FIdent f = "and_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> 
            let@ x' = dis_expr' loc x in
            (match x' with
            | Val v -> 
                if to_bool loc v then dis_expr' loc y else DisEnv.pure sym_false
            | Exp e -> 
                let+ y' = dis_expr' loc y in
                (Exp (Expr_TApply(f, tes, [e; to_expr y'])))
            )
        | _ ->
            raise (EvalError (loc, "malformed and_bool expression"))
        )
    end else if name_of_FIdent f = "or_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> 
            let@ x' = dis_expr' loc x in
            (match x' with
            | Val v -> 
                if to_bool loc v then DisEnv.pure sym_true else dis_expr' loc y
            | Exp e -> 
                let+ y' = dis_expr' loc y in
                (Exp (Expr_TApply(f, tes, [e; to_expr y'])))
            )
        | _ ->
            raise (EvalError (loc, "malformed or_bool expression"))
        )
    end else if name_of_FIdent f = "implies_bool" then begin
        (match (tes, es) with
        | ([], [x; y]) -> 
            let@ x' = dis_expr' loc x in
            (match x' with
            | Val v -> 
                if to_bool loc v then dis_expr' loc y else DisEnv.pure sym_true
            | Exp e -> 
                let+ y' = dis_expr' loc y in
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
                let@ v' = DisEnv.getVar loc arg in
                let@ () = DisEnv.modify (LocalEnv.addImplicitValue arg v') in

                let@ e' = dis_expr' loc e in
                match e' with
                | Val v -> DisEnv.modify (LocalEnv.addLocalVar loc arg v)
                | Exp _ -> DisEnv.unit
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
            let@ rv = (match rty with 
                | Some (Type_Tuple ts) -> 
                    let@ num = DisEnv.incNumSymbols in
                    let varNames = List.map (fun _ -> Ident (fName ^ "Var" ^ string_of_int num)) ts in
                    let symbol = (Expr_Tuple(List.map (fun n -> Expr_Var n) varNames)) in
                    let@ () = DisEnv.modify (LocalEnv.addReturnSymbol symbol) in

                    let@ ts' = DisEnv.traverse (dis_type loc) ts in
                    let+ () = DisEnv.write (List.map2 (fun t' name ->
                        (Stmt_VarDeclsNoInit(
                            t',
                            [name], 
                            Unknown
                        ))
                    ) ts' varNames) in
                    symbol
                | Some t -> 
                    let@ num = DisEnv.incNumSymbols in
                    let varName = Ident (fName ^ "Var" ^ string_of_int num) in
                    let symbol = (Expr_Var(varName)) in
                    let@ () = DisEnv.modify (LocalEnv.addReturnSymbol symbol) in

                    let@ t' = dis_type loc t in
                    let+ () = DisEnv.write [Stmt_VarDeclsNoInit(
                        t',
                        [varName], 
                        Unknown
                    )] in
                    symbol
                | None -> raise (EvalError (loc, "Unexpected function return type")))
            in

            (* Get the return symbol we just made so we can return it later *)
            (* let@ rv = DisEnv.gets (LocalEnv.getReturnSymbol loc) in *)

            (* Add local parameters, avoiding name collisions *)
            (* Also print what the parameter refers to *)
            let@ num = DisEnv.incNumSymbols in
            let localPrefix = fName ^ string_of_int num in
            let@ () = DisEnv.sequence_ (Utils.map3 (fun (ty, _) arg ex -> 
                let@ ex' = dis_expr' loc ex in 
                let v = match ex' with 
                | Val v -> v
                | Exp _ -> VUninitialized in
                let name = (Ident (localPrefix ^ pprint_ident arg)) in
                let@ () = DisEnv.modify (LocalEnv.addLocalVar loc  name v) in
                let@ ty' = dis_type loc ty in
                DisEnv.write [Stmt_VarDecl(ty', Ident (localPrefix ^ (pprint_ident arg)), to_expr ex', loc)]
                
            ) atys args es) in

            (* print out the body *)
            let@ () = DisEnv.modify (LocalEnv.addLocalPrefix localPrefix) in
            let@ () = dis_stmts env b in
            let@ () = DisEnv.modify (LocalEnv.removeReturnSymbol)
            and@ () = DisEnv.modify (LocalEnv.removeLocalPrefix) in

            (* Restore implicit values *)
            List.iter (fun (arg, v) -> Env.addLocalVar loc env arg v) (Env.getImplicitLevel env);

            (* Return the return symbol to be used in expressions e.g. assignments *)
            return (Exp rv)
        | None ->
            let* tes' = dis_exprs loc env tes in
            let* es' = dis_exprs loc env es in
            (match eval_prim 
                (name_of_FIdent f) 
                (List.map (fun v -> match v with | Val v -> v | Exp _ -> VUninitialized) tes') 
                (List.map (fun v -> match v with | Val v -> v | Exp _ -> VUninitialized) es') with
            | Some VUninitialized -> return (Exp (Expr_TApply(f, List.map to_expr tes', List.map to_expr es')))
            | Some v -> return (Val v)
            | None -> return (Exp (Expr_TApply(f, List.map to_expr tes', List.map to_expr es')))
            )
        )

(** Dissassemble expression. This should never return Result VUninitialized *)
and dis_expr (loc: l) (env: Env.t) (x: AST.expr): sym writer =
    match x with
    | Expr_If(c, t, els, e) ->
        let rec eval_if xs d = match xs with
            | [] -> dis_expr loc env d
            (* If we cannot evaluate the condition, print the whole statement *)
            | AST.E_Elsif_Cond (cond, b)::xs' -> 
                let* cond' = dis_expr loc env cond in
                match cond' with
                | Val v ->
                    if to_bool loc v then
                        (* Just print this branch *)
                        dis_expr loc env b
                    else
                        (* Print whatever the rest of the branches turn out to be *)
                        eval_if xs' d
                | Exp cond'' -> 
                    let tEnv = Env.copy env in
                    let elsEnv = List.map (fun _ -> Env.copy env) (Utils.range 0 (List.length els)) in
                    let eEnv = Env.copy env in
                    let* b' = Env.nest (fun env' -> dis_expr loc env' b) tEnv in
                    let* els' = dis_if_expr_no_remove loc elsEnv els in
                    let* e' = Env.nest (fun env' -> dis_expr loc env' e) eEnv in
                    Env.setLocals env (List.fold_left mergeEnv (Env.getLocals tEnv) ((Env.getLocals eEnv)::(List.map Env.getLocals elsEnv)));
                    return (Exp (Expr_If(
                        cond'', 
                        to_expr (b'),
                        els',
                        to_expr (e')
                    )))
        in
        eval_if (E_Elsif_Cond(c, t)::els) e
    (* NOTE: This does not consider early returns currently. It also doesn't handle recursive calls *)
    | Expr_TApply(f, tes, es) ->
        dis_fun loc env f tes es
    | Expr_Var id ->
        (try 
            (match (Env.getVar loc env (Ident ((Env.getLocalPrefix loc env) ^ pprint_ident id))) with 
            | VUninitialized -> return (Exp (Expr_Var(Ident ((Env.getLocalPrefix loc env) ^ pprint_ident id))))
            | v -> return (Val v))
        with EvalError _ ->
            (try 
                (match (Env.getVar loc env id) with 
                | VUninitialized -> return (Exp x)
                | v -> return (Val v))
            with EvalError _ -> return (Exp x)))
    | Expr_In(e, p) ->
        let* e' = dis_expr loc env e in
        (match e' with
        | Val v -> return (Val (from_bool (eval_pattern loc env v p)))
        | Exp _ -> return (Exp x))
    | Expr_Slices(e, ss) ->
        let** transformedSlices = List.map (fun s -> dis_slice loc env s) ss in
        let* e' = dis_expr loc env e in
        (match e' with
        | Val v ->
            if List.exists (fun ts -> match ts with (Exp _, _) -> true | (_, Exp _) -> true | (_, _) -> false) transformedSlices then
                return (Exp (Expr_Slices(to_expr (Val v), List.map (fun (i, w) -> Slice_HiLo(to_expr i, to_expr w)) transformedSlices)))
            else
                let vs = List.map (fun s -> 
                    (match s with
                    | (Val v1, Val v2) -> extract_bits loc v v1 v2
                    | _ -> raise (EvalError (loc, "Unreachable: Shouldn't have expression in bit slice\n")))
                    ) transformedSlices in
                    return (Val (eval_concat loc vs))
        | Exp e' -> 
            return (Exp (Expr_Slices(e', List.map2 (fun (i, w) s ->
                (match s with
                | Slice_Single _ -> Slice_Single(to_expr i)
                | Slice_HiLo _ -> Slice_LoWd(to_expr i, to_expr w)
                | Slice_LoWd _ -> Slice_LoWd(to_expr i, to_expr w)
                )
            ) transformedSlices ss))))
    | Expr_Tuple(es) ->
        let** transformedExprs = List.map (dis_expr loc env) es in
        if contains_expr transformedExprs then
            return (Exp (Expr_Tuple(List.map to_expr transformedExprs)))
        else
            return (Val (VTuple (List.map to_value transformedExprs)))
    | Expr_Array(a, i) ->
        let* a' = dis_expr loc env a in
        let* i' = dis_expr loc env i in
        if is_expr a' || is_expr i' then
            return (Exp (Expr_Array(a, to_expr i')))
        else
            (match (get_array loc (to_value a') (to_value i')) with
            | VUninitialized -> return (Exp (Expr_Array(a, to_expr i')))
            | v -> return (Val v))
    | x -> try (match eval_expr loc env x with VUninitialized -> return (Exp x) | v -> return (Val v)) with EvalError (loc, message) -> return (Exp x)

(** Evaluate and simplify guards and bodies of an elseif chain, without removing branches *)
and dis_if_expr_no_remove (loc: l) (envs: Env.t list) (xs: e_elsif list): e_elsif list writer = 
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
    | _ -> raise (EvalError (loc, "Env and e_elsif list must have the same length"))

and dis_lexpr (loc: l) (env: Env.t) (x: AST.lexpr) (r: sym): unit writer =
    match x with
    | LExpr_Write(setter, tes, es) ->
        let** tvs = List.map (dis_expr loc env) tes in
        let** vs = List.map (dis_expr loc env) es in
        if contains_expr tvs || contains_expr vs || contains_expr [r] then
            write (Stmt_Assign(LExpr_Write(setter, List.map to_expr tvs, List.map to_expr vs), to_expr r, loc))
        else begin
            return (eval_proccall 
            loc 
            env 
            setter 
            (List.map (to_value) tvs) 
            ((List.map to_value vs) @ [to_value r]))
        end
    | LExpr_Var(v) ->
        let v' = try 
            (ignore (Env.getVar loc env (Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v)))); 
            (Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v)))) 
        with EvalError _ -> v in
        (match r with
        | Val r' -> 
            return (Env.setVar loc env v' r')
        | Exp e -> 
            Env.setVar loc env v' VUninitialized;
            write (Stmt_Assign(LExpr_Var(v'), e, loc)))
    | LExpr_Wildcard -> return ()
    | _ -> try (return (eval_lexpr loc env x (to_value r))) with EvalError _ ->
        write (Stmt_Assign(x, to_expr r, loc))

(** Dissassemble list of statements *)
and dis_stmts (env: Env.t) (xs: AST.stmt list): unit writer =
    write_multi (List.concat (List.map read (List.map (dis_stmt env) xs)))

(** Evaluate and simplify guards and bodies of an elseif chain, without removing branches *)
and dis_if_stmt_no_remove (loc: l) (envs: Env.t list) (xs: s_elsif list): s_elsif list writer = 
    match (xs, envs) with
    | ([], []) -> return []
    | ((AST.S_Elsif_Cond (cond, b)::xs'), (env::envs')) ->
        let* cond' = dis_expr loc env cond in
        let* els' = dis_if_stmt_no_remove loc envs' xs' in
        let b' = Env.nest (fun env' -> dis_stmts env' b) env in
        return (AST.S_Elsif_Cond(to_expr cond', read (b'))::(els'))
    | _ -> raise (EvalError (loc, "Env list and s_elsif list must be the same length"))

(** Disassemble statement *)
and dis_stmt (env: Env.t) (x: AST.stmt): unit writer =
    (match x with
    | Stmt_VarDeclsNoInit(ty, vs, loc) ->
        (* If a local prefix exists, add it *)
        let vs' = try (List.map (fun v -> Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v))) vs) with EvalError _ -> vs in
        (* Add the variables *)
        List.iter (fun v -> Env.addLocalVar loc env v VUninitialized) vs';
        (* Print the declarations *)
        let* ty' = dis_type loc env ty in
        write (Stmt_VarDeclsNoInit(ty', vs', loc))
    | Stmt_VarDecl(ty, v, i, loc) ->
        (* If a local prefix exists, add it *)
        let v' = try Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v)) with EvalError _ -> v in
        (* Add the variable *)
        let* i' = dis_expr loc env i in
        (match i' with
        | Val i' -> return (Env.addLocalVar loc env v' i')
        | Exp ex ->
            Env.addLocalVar loc env v' VUninitialized;
            let* ty' = dis_type loc env ty in
            write (Stmt_VarDecl(ty', v', ex, loc))
        )
    | Stmt_ConstDecl(ty, v, i, loc) ->
        (* If a local prefix exists, add it *)
        let v' = try Ident ((Env.getLocalPrefix loc env) ^ (pprint_ident v)) with EvalError _ -> v in
        let* i' = dis_expr loc env i in
        (match i' with
        | Val i' -> return (Env.addLocalConst loc env v' i')
        | Exp ex -> 
            (* Declare constant with uninitialized value and just use symbolically *)
            Env.addLocalConst loc env v' VUninitialized;
            let* ty' = dis_type loc env ty in
            write (Stmt_ConstDecl(ty', v', ex, loc))
        )
    | Stmt_Assign(l, r, loc) ->
        let* r' = dis_expr loc env r in
        (match (l, r') with
        | (LExpr_Tuple(les), Exp Expr_Tuple(es)) ->
            let** _ = List.map2 (fun le e -> dis_stmt env (Stmt_Assign(le, e, loc))) les es in
            return ()
        | _ ->
            dis_lexpr loc env l r' (* TODO: double check that both statements are added *)
        )
    | Stmt_If(c, t, els, e, loc) ->
        let rec eval_if xs d = match xs with
            | [] -> dis_stmts env e
            | AST.S_Elsif_Cond (cond, b)::xs' ->
                let* cond' = dis_expr loc env cond in
                (match cond' with
                | Val v -> 
                    if to_bool loc v then
                        (* Just print this branch *)
                        dis_stmts env b
                    else
                        (* Print whatever the rest of the branches turn out to be *)
                        eval_if xs' d
                (* We have to print out all branches now because we don't know
                   whether or not this one is true. We can still simplify 
                   guards and bodies though *)
                | Exp cond'' -> 
                    let tEnv = Env.copy env in
                    let elsEnv = List.map (fun _ -> Env.copy env) (Utils.range 0 (List.length els)) in
                    let eEnv = Env.copy env in
                    let t' = read (Env.nest (fun env' -> dis_stmts env' t) tEnv) in
                    let* els' = dis_if_stmt_no_remove loc elsEnv els in
                    let e' = read (Env.nest (fun env' -> dis_stmts env' e) eEnv) in
                    Env.setLocals env (List.fold_left mergeEnv (Env.getLocals tEnv) ((Env.getLocals eEnv)::(List.map Env.getLocals elsEnv)));
                    write (Stmt_If(cond'', t', els', e', loc))
                )
        in
        eval_if (S_Elsif_Cond(c, t)::els) e
    | Stmt_FunReturn(e, loc) ->
        let* e' = dis_expr loc env e in
        (match (e', Env.getReturnSymbol loc env) with
        | (Exp (Expr_Tuple(es)), Expr_Tuple(les)) ->
            write_multi (List.map2 (fun le e -> 
                Stmt_Assign((match le with Expr_Var(ident) -> LExpr_Var(ident) | _ -> raise (EvalError (loc, "Unexpected expression type in return symbol"))), e, loc)
            ) les es)
        | (_, Expr_Var(i)) -> write (Stmt_Assign(LExpr_Var(i), to_expr e', loc))
        | _ -> raise (EvalError (loc, "TODO"))
        )
    | Stmt_Assert(e, loc) ->
        let* e' = dis_expr loc env e in
        (match e' with 
        | Val v -> return (if not (to_bool loc v) then
            raise (EvalError (loc, "assertion failure")))
        | Exp e'' ->
            write (Stmt_Assert(e'', loc))
        )
    | Stmt_Case(e, alts, odefault, loc) ->
        (let rec eval v alts =
            (match alts with
            | [] ->
                    (match odefault with
                    | None -> raise (EvalError (loc, "unmatched case"))
                    | Some s -> dis_stmts env s
                    )
            | (Alt_Alt(ps, oc, s) :: alts') ->
                    if List.exists (eval_pattern loc env v) ps && Utils.from_option
                    (Utils.map_option (to_bool loc) (Utils.map_option (eval_expr loc env) oc)) (fun _ -> true) then
                        dis_stmts env s
                    else
                        eval v alts'
            )
        in
        let* e' = dis_expr loc env e in
        (match e' with
        | Val v -> eval v alts
        | Exp e'' ->
            let altEnvs = List.map (fun _ -> Env.copy env) (Utils.range 0 (List.length alts)) in
            let defEnv = Env.copy env in
            let result = write (Stmt_Case(
                e'', 
                List.map2 (fun (Alt_Alt(ps, oc, s)) altEnv -> Alt_Alt(ps, oc, read (Env.nest (fun env' -> (dis_stmts env' s)) altEnv))) alts altEnvs, 
                (match odefault with None -> None | Some s -> Some (read (Env.nest (fun env' -> (dis_stmts env s)) defEnv))), 
                loc
            )) in
            Env.setLocals env (List.fold_left mergeEnv (Env.getLocals defEnv) (List.map Env.getLocals altEnvs));
            result
        ))
    | x -> write x
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
                    let stmts = read (dis_stmts env exec) in
                    (* List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) stmts; *)
                    (* List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) (join_decls (remove_unused (copy_propagation (constant_propagation stmts)))); *)
                    (* Some stmts *)
                    Some (join_decls (remove_unused (copy_propagation (constant_propagation stmts))));
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
