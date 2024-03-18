open Asl_ast
open Asl_utils
open Asl_visitor
open Utils

type state = {
  mutable callers : IdentSet.t Bindings.t;
  mutable seen : IdentSet.t;
  mutable worklist : IdentSet.t;
}

class call_visitor(fn : ident -> unit) = object (self)
  inherit Asl_visitor.nopAslVisitor
  method! vlexpr e =
    (match e with
    | LExpr_Write (f, _, _) -> fn f
    | LExpr_ReadWrite (g, s, _, _) -> fn g; fn s
    | _ -> ());
    DoChildren
  method! vexpr e =
    (match e with
    | Expr_TApply (f, _, _) -> fn f
    | _ -> ());
    DoChildren
  method! vstmt e =
    (match e with
    | Stmt_TCall (f, _, _, _) -> fn f
    | _ -> ());
    DoChildren
end

let init_state i: state =
  { callers = Bindings.empty; seen = i; worklist = i }

let get_callers (st: state) id =
  match Bindings.find_opt id st.callers with
  | None -> IdentSet.empty
  | Some v -> v

let callback (st: state) (caller: ident) (callee: ident) =
  (* Add caller edge *)
  let existing = get_callers st callee in
  st.callers <- Bindings.add callee (IdentSet.add caller existing) st.callers;
  (* Add to worklist if a new callee *)
  if not (IdentSet.mem callee st.seen) then st.worklist <- IdentSet.add callee st.worklist;
  (* Mark as seen *)
  st.seen <- IdentSet.add callee st.seen

let get_body i env =
  match Eval.Env.getFunOpt Unknown env i with
  | Some fnsig -> fnsig_get_body fnsig
  | _ -> []

let run (init: IdentSet.t) frontier (env: Eval.Env.t): (IdentSet.t Bindings.t * IdentSet.t) =
  (* create mutable state with initial worklist, seen, empty edges *)
  let rec iter st = begin
    (* Get fns to visit, clear worklist *)
    let delta = st.worklist in
    st.worklist <- IdentSet.empty;

    (* Walk each function in delta *)
    IdentSet.iter (fun fn ->
      let walker = new call_visitor(callback st fn) in
      let body = get_body fn env in
      let _ = visit_stmts walker body in
      ()) (IdentSet.diff delta frontier);

    (* If more fns to process, loop *)
    if IdentSet.cardinal st.worklist = 0 then ()
    else iter st
  end in

  let st = init_state init in
  let _ = iter st in
  (* Filter seen to only include functions with implementations *)
  let seen = IdentSet.filter (fun v -> not (isNone (Eval.Env.getFunOpt Unknown env v))) st.seen in
  let seen = IdentSet.diff seen frontier in
  (st.callers, seen)

