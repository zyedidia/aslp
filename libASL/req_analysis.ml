open Asl_ast
open Asl_utils
open Symbolic

(*
  Given a function, determine the constraints on its local variables and incoming arguments such that loop bounds
  and bitvector widths would be known.
  Assumes all type arguments must be known, as an initial interprocedural abstraction, but gradually extends this.
  Once this is complete, expressions that must be known are specialised by generating an If statement that dis will fully
  expand, specialising all subsequent statements.

  It is definitely not sound in its current form, simply tracking enough information to get the instruction sets to go through dis.
  As dis becomes more capable, hopefully this stage will not be required.
*)

type req =
  Concrete |
  BitRange of Z.t * int |
  NoReq

let pp_req = function
  | Concrete -> "Concrete"
  | BitRange (bv,n) -> "BitRange(" ^ Z.format ("%0" ^ string_of_int n ^ "b") bv ^ "," ^ string_of_int (Z.popcount bv) ^ ")"
  | NoReq -> "NoReq"

let join a b =
  match a, b with
  | Concrete, _ 
  | _, Concrete -> Concrete
  | BitRange(a,n), BitRange(b,m) ->
      let w = max n m in
      BitRange (Z.logor a b,w)
  | BitRange (b, wd), NoReq 
  | NoReq, BitRange (b, wd) -> BitRange (b, wd)
  | NoReq, NoReq -> NoReq

type state = {
  reqs: req Bindings.t;
  ctx: req;
  fn_reqs: (req list) Bindings.t;
  callers: IdentSet.t Bindings.t;
}

let state_join a b =
  { reqs = Bindings.merge (fun k a b ->
    match a, b with
    | Some a, Some b -> Some (join a b)
    | Some a, None
    | None, Some a -> Some a
    | _ -> None) a.reqs b.reqs;
    ctx = join a.ctx b.ctx ;
    fn_reqs = a.fn_reqs;
    callers = a.callers }

type 'a wrm = (state -> (state * stmt list * 'a,string) Either.t)
let (let@) x f = fun s ->
  match x s with
  | Either.Left(s,w,r) ->
      (match f r s with
      | Either.Left(s,w',r) -> Either.Left(s,w@w',r)
      | Either.Right e -> Either.Right e)
  | Either.Right e -> Either.Right e
let (let+) x f = fun s ->
  match x s with
  | Either.Left(s,w,r) -> Either.Left (s,w,f r)
  | Either.Right e -> Either.Right e
let wrap f = fun s ->
  match f s with
  | Either.Left r -> Either.Left (s,[],r)
  | Either.Right e -> Either.Right e
let write l = fun s ->
  Either.Left (s,[l],())
let writel l = fun s ->
  Either.Left (s,l,())
let pure v = fun s ->
  Either.Left (s,[],v)
let fail m = fun s ->
  Either.Right m
let assert_ c m =
  if not c then fail m else pure ()
let rec split f l =
  match l with
  | [] -> pure ([], [])
  | x::xs -> 
      let@ b = f x in
      let+ (l,r) = split f xs in
      if b then (x::l,r) else (l,x::r)
let rec traverse (f: 'a -> 'b wrm) (l: 'a list): ('b list) wrm =
  match l with
  | [] -> pure []
  | x::xs ->
      let@ x = f x in
      let+ xs = traverse f xs in
      x::xs
let traverse_ f l =
  let+ _ = traverse f l in ()
let rec traverse2_ (f: 'a -> 'b -> unit wrm) (l: 'a list) (l2: 'b list): unit wrm =
  match l, l2 with
  | [], [] -> pure ()
  | x::xs, y::ys -> 
      let@ _ = f x y in
      traverse2_ f xs ys
  | _, _ -> invalid_arg "traverse2_"

let with_state f = fun s ->
  Either.Left (s,[],f s)

let update_state f = fun s ->
  Either.Left (f s,[],())

let get_req v = with_state (fun s ->
  match Bindings.find_opt v s.reqs with
  | Some r -> r
  | None -> NoReq)

let get_fn_reqs f = with_state (fun s -> Bindings.find_opt f s.fn_reqs)

let get_callers f = with_state (fun s -> match Bindings.find_opt f s.callers with Some v -> v | _ -> IdentSet.empty)

let add_req v r = fun s ->
  match Bindings.find_opt v s.reqs with
  | Some r' ->
      let reqs = Bindings.add v (join r r') s.reqs in
      Either.Left({s with reqs},[],())
  | None -> 
      let reqs = Bindings.add v r s.reqs in
      Either.Left({s with reqs},[],())

let update_fn_reqs f v = update_state (fun s ->
  let fn_reqs = Bindings.add f v s.fn_reqs in
  {s with fn_reqs})

let influence_ctx v = update_state (fun s ->
  let ctx = join v s.ctx in
  {s with ctx})

let clear_ctx = update_state (fun s ->
  let ctx = NoReq in
  {s with ctx})

let get_ctx = with_state (fun s -> s.ctx)

let to_int e =
  match e with
  | Expr_LitInt i -> Some (int_of_string i)
  | _ -> None

let rec visit_expr e r =
  match e with
  | Expr_Var v -> add_req v r

  | Expr_Slices(e, [Slice_LoWd(lo, wd)]) ->
      (match r, to_int lo, to_int wd with
      | Concrete, Some l, Some w -> 
          let n = l + w in
          assert (w > 0);
          let m = Z.pred (Z.pow (Z.succ Z.one) w) in
          let b = Z.shift_left m l in
          visit_expr e (BitRange(b,n))
      | _ -> visit_expr e r)
  | Expr_Slices(e, _) -> visit_expr e r

  | Expr_If(ty, c, t, els, f) ->
      let@ _ = visit_expr c r in
      let@ _ = visit_expr t r in
      let@ _ = visit_expr f r in
      pure ()

  | Expr_TApply(f, tes, es) ->
      let@ _ = traverse (fun v -> visit_expr v Concrete) tes in
      let@ req = get_fn_reqs f in
      (match req with
      | Some rs -> 
          let rs = List.map (join r) rs in
          traverse2_ visit_expr es rs 
      | None -> traverse_ (fun v -> visit_expr v r) es)

  | Expr_Tuple(es) -> traverse_ (fun v -> visit_expr v r) es
  | Expr_Parens(e) -> visit_expr e r

  | Expr_In(e,p) -> visit_expr e r

  | Expr_LitHex _
  | Expr_LitInt _
  | Expr_LitReal _
  | Expr_LitBits _
  | Expr_LitString _ -> pure ()

  | Expr_ImpDef _
  | Expr_Array _
  | Expr_Field _
  | Expr_Fields _
  | Expr_Unknown _ -> pure ()

  | _ -> fail @@ "visit_expr: " ^ (pp_expr e)

and visit_lexpr l e =
  match l with
  | LExpr_Var v ->
      let@ r = get_req v in
      let@ _ = visit_expr e r in
      influence_ctx r

  | LExpr_Wildcard -> pure ()

  | LExpr_Tuple(ls) -> traverse_ (fun l -> visit_lexpr l e) ls

  | LExpr_Field(l,f) -> let+ _ = visit_expr e NoReq in ()
  | LExpr_Fields(l,f) -> let+ _ = visit_expr e NoReq in ()
  | LExpr_Array(l, i) ->
      let@ _ = visit_expr i NoReq in
      visit_lexpr l e

  | LExpr_Slices(l,f) -> visit_lexpr l e

  | LExpr_Write(f, tes, es) ->
      let@ () = traverse_ (fun v -> visit_expr v Concrete) tes in
      let@ req = get_fn_reqs f in
      (match req with
      | Some rs -> traverse2_ visit_expr (es@[e]) rs 
      | None -> traverse_ (fun v -> visit_expr v NoReq) es)

  | _ -> fail @@ "visit_lexpr: " ^ (pp_lexpr l)

and visit_type ty =
  match ty with
  | Type_Bits e -> visit_expr e Concrete
  | _ -> pure ()

and visit_stmt s =
  match s with
  | Stmt_VarDeclsNoInit(ty, vs, loc) ->
      visit_type ty

  | Stmt_ConstDecl(ty, v, e, loc) ->
      let@ r = get_req v in
      visit_expr e r

  | Stmt_VarDecl(ty, v, e, loc) ->
      let@ r = get_req v in
      visit_expr e r

  | Stmt_Assign(l, e, loc) ->
      visit_lexpr l e 

  | Stmt_Assert(e, loc) ->
      visit_expr e NoReq

  | Stmt_TCall(f, tes, es, loc) ->
      let@ _ = traverse (fun v -> visit_expr v Concrete) tes in
      let@ req = get_fn_reqs f in
      (match req with
      | Some rs -> traverse2_ visit_expr es rs 
      | None -> pure ())

  | Stmt_ProcReturn(loc) ->
      pure ()

  | Stmt_FunReturn(e, loc) ->
      visit_expr e NoReq

  (* Missing some expressions here *)
  | Stmt_If(c, t, els, f, loc) ->
      let@ (ts,_,_) = wrap (let@ _ = clear_ctx in visit_stmts t) in
      let@ (fs,_,_) = wrap (let@ _ = clear_ctx in visit_stmts f) in
      let@ _ = update_state (fun _ -> state_join ts fs) in
      pure ()
  | Stmt_Case(c, alts, odefault, loc) ->
      let@ (d,_,_) = wrap (match odefault with Some b -> visit_stmts b | None -> pure ()) in
      let@ a = traverse (function (Alt_Alt(_,_,b)) -> wrap (let@ _ = visit_stmts b in pure ())) alts in
      visit_expr c NoReq

  (* Technicaly should be FP - no cases where it really matters though *)
  | Stmt_For(v, start, dir, stop, b, loc) ->
      let@ _ = visit_stmts b in
      let@ _ = visit_expr start Concrete in
      visit_expr stop Concrete
  | Stmt_While(c, b, loc) ->
      let@ _ = visit_stmts b in
      visit_expr c Concrete

  | Stmt_Unpred(_)
  | Stmt_ConstrainedUnpred(_)
  | Stmt_ImpDef(_, _) 
  | Stmt_ExceptionTaken(_)
  | Stmt_Dep_Unpred(_)
  | Stmt_Dep_ImpDef(_, _)
  | Stmt_Dep_Undefined(_) 
  | Stmt_See(_, _)
  | Stmt_Throw(_, _)
  | Stmt_Undefined(_) -> pure ()

  | _ -> fail @@ "visit_stmt: " ^ (pp_stmt s)

and visit_stmts s =
  let+ _ = traverse visit_stmt (List.rev s) in
  ()

and visit_fn fn targs args body =
  let@ () = visit_stmts body in
  let@ reqs = traverse get_req args in
  let@ old_reqs = get_fn_reqs fn in
  match old_reqs with
  | Some old when reqs = old -> pure IdentSet.empty
  | None when List.for_all (fun v -> v = NoReq) reqs -> pure IdentSet.empty
  | _ -> 
      let@ () = update_fn_reqs fn reqs in
      get_callers fn

let rec run_fn_set (delta: IdentSet.t) (fn_reqs: (req list) Bindings.t) (fn_vars: req Bindings.t Bindings.t) fns (callers: IdentSet.t Bindings.t) =
  let (delta,fn_reqs,fn_vars) = IdentSet.fold (fun fn (delta,fn_reqs,fn_vars) ->
    match Bindings.find_opt fn fns with
    | None -> (delta,fn_reqs,fn_vars)
    | Some fnsig ->
        let st = { reqs = Bindings.empty; ctx = NoReq; fn_reqs ; callers } in
        match visit_fn fn (fnsig_get_targs fnsig) (fnsig_get_args fnsig) (fnsig_get_body fnsig) st with
        | Left (st,_,modset) -> (IdentSet.union modset delta, st.fn_reqs, Bindings.add fn st.reqs fn_vars)
        | Right m -> 
            Printf.printf "Gen_reqs: Failed for '%s': %s\n" (pprint_ident fn) m;
            (delta,fn_reqs,fn_vars)
  ) delta (IdentSet.empty,fn_reqs,fn_vars) in
  if IdentSet.cardinal delta = 0 then (fn_reqs, fn_vars)
  else run_fn_set delta fn_reqs fn_vars fns callers

let rec possible_int = function
  | Expr_LitInt w -> Some (int_of_string w)
  | Expr_TApply (FIdent("add_int",0), [], [a;b]) ->
      (match (possible_int a), (possible_int b) with
      | Some a, Some b -> Some ((a + b))
      | _, _ -> None)
  | Expr_TApply (FIdent("sub_int",0), [], [a;b]) ->
      (match (possible_int a), (possible_int b) with
      | Some a, Some b -> Some ((a - b))
      | _, _ -> None)
  | Expr_TApply (FIdent("mul_int",0), [], [a;b]) ->
      (match (possible_int a), (possible_int b) with
      | Some a, Some b -> Some ((a * b))
      | _, _ -> None)
  | _ -> None

let rec collapse_e_if e =
  match e with
  | Expr_If(_,c,t,els,f) -> E_Elsif_Cond(c,t)::els@(collapse_e_if f)
  | _ -> [E_Elsif_Cond(expr_true, e)]

let rec enumerate e =
  match e with
  (* Enumerate the range of HighestSetBit if we know the width already *)
  | Expr_TApply (FIdent (("HighestSetBit" | "LowestSetBit"), 0), [w], [a]) ->
      (match (possible_int w) with
      | Some w -> 
          let vals = List.init (w+1) (fun i -> if i = w then -1 else i) in
          Some (List.map (fun i -> 
            let v = Expr_LitInt (string_of_int i) in
            (Expr_TApply (FIdent ("eq_int", 0), [], [e; v]), v )) vals)
      | None -> None)

  (* Enumerate the cases of UInt *)
  | Expr_TApply (FIdent ("UInt", 1), [ws], [a]) ->
      (match possible_int ws with
      | Some w ->
          Some (List.init (Z.to_int (Z.pow (Z.succ Z.one) w)) (fun i -> 
            let v = Expr_LitInt (string_of_int i) in
            let bv = Expr_LitBits (Z.format ("%0" ^ string_of_int w ^ "b") (Z.of_int i)) in
            (Expr_TApply (FIdent ("eq_bits", 0), [ws], [bv;a]),v)))
      | None -> None)

  (* Enumerate the cases of an ITE *)
  | Expr_If _ ->
      let els = collapse_e_if e in
      Some (List.map (function E_Elsif_Cond(c,b) -> (c,b)) els) 

  (* Search the arguments to shift and add for structure we can enumerate *)
  | Expr_TApply (FIdent ("shift_left_int", 0), [], [a;b]) ->
      (match enumerate b with
      | Some w -> Some (List.map (fun (test,expr) -> (test,Expr_TApply (FIdent ("shift_left_int", 0), [], [a; expr]))) w)
      | None -> None)
  | Expr_TApply (FIdent ("add_int", 0), [], [a;b]) ->
      (match enumerate b with
      | Some w -> Some (List.map (fun (test,expr) -> (test,Expr_TApply (FIdent ("add_int", 0), [], [a; expr]))) w)
      | _ -> (match enumerate a with
              | Some w -> Some (List.map (fun (test,expr) -> (test,Expr_TApply (FIdent ("add_int", 0), [], [b; expr]))) w)
              | None -> None))
 
  | _ -> None

let contains_req_assign s : bool wrm =
  let+ search = traverse (fun s ->
    match s with
    | Stmt_Assign(LExpr_Var v, _, _) -> let+ r = get_req v in r <> NoReq
    | _ -> pure false) s in
  List.mem true search

let fix_stmt fid s =
  match s with
  | Stmt_VarDecl(ty, v, e, loc) 
  | Stmt_ConstDecl(ty, v, e, loc) ->
      let@ r = get_req v in
      let vals = enumerate e in
      (match r, vals with
      | NoReq, _
      | _, None -> write s
      | _, Some vals ->
          Printf.printf "  %s: Splitting %s into %d values\n" (name_of_FIdent fid)(pp_stmt s) (List.length vals);
          let@ _ = write (Stmt_VarDeclsNoInit(ty, [v], loc)) in
          write (List.fold_right (fun (test,expr) acc -> 
            Stmt_If(test, [Stmt_Assign(LExpr_Var v, expr, loc)], [], [acc], loc)) vals (Stmt_Throw (Ident ("UNREACHABLE"), loc))))
  | Stmt_Case(v, alts, None, loc) ->
      let@ b = contains_req_assign (List.flatten (List.map (function Alt_Alt(p,oc,b) -> b) alts)) in
      if not b then write s
      else
        (Printf.printf "  %s: Splitting %s into %d values\n" (name_of_FIdent fid) (pp_stmt s) (List.length alts);
        write (List.fold_left (fun acc (Alt_Alt(p,oc,b)) ->
          let e = Expr_In (v, Pat_Set p) in
          let e = match oc with Some c -> Expr_TApply (FIdent ("and_bool", 0), [], [e;c]) | _ -> e in
          Stmt_If(e, b, [], [acc], loc)) (Stmt_Throw (Ident ("UNREACHABLE"), loc))  alts))
  | Stmt_Case(v, alts, Some d, loc) ->
      let@ b = contains_req_assign (List.flatten (List.map (function Alt_Alt(p,oc,b) -> b) alts)) in
      if not b then write s
      else
        (Printf.printf "  %s: Splitting %s into %d values\n" (name_of_FIdent fid) (pp_stmt s) (List.length alts + 1);
        writel (List.fold_left (fun acc (Alt_Alt(p,oc,b)) ->
          let e = Expr_In (v, Pat_Set p) in
          let e = match oc with Some c -> Expr_TApply (FIdent ("and_bool", 0), [], [e;c]) | _ -> e in
          [Stmt_If(e, b, [], acc, loc)]) (d)  alts))
  | _ -> write s

let fix_stmts fid s =
  let+ _ = traverse (fix_stmt fid) s in
  () 

let run fns callers = 
  let delta = Bindings.fold (fun f _ -> IdentSet.add f) fns IdentSet.empty in
  let (sigs,vars) = run_fn_set delta Bindings.empty Bindings.empty fns callers in
  Bindings.mapi (fun fn fnsig -> 
    match Bindings.find_opt fn vars with
    | None -> fnsig
    | Some v ->
        (*Printf.printf "%s\n" (name_of_FIdent fn);
        Bindings.iter (fun v req -> Printf.printf "  %s -> %s\n" (pprint_ident v) (pp_req req)) v; *)
        let st = { reqs = v; fn_reqs = sigs ; ctx = NoReq; callers } in
        match fix_stmts fn (fnsig_get_body fnsig) st with
        | Left (_,b,_) -> fnsig_set_body fnsig b
        | _ -> fnsig) fns
