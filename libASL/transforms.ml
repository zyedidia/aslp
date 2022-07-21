open Asl_utils
open AST

module RemoveUnused = struct

  module M = Rws.Make(struct
    type r = unit
    type w = stmt list
    type s = IdentSet.t

    let mappend = (@)
    let mempty = []
  end)

  include M.Let

  let rec remove_unused' (xs: stmt list): unit M.rws =
    M.traverse_ (fun stmt ->
      let* used = M.get in

      let include_stmt (s: stmt): unit M.rws =
        let* () = M.write [s] in
        M.modify (IdentSet.union (fv_stmt s)) in

      match stmt with
      | Stmt_VarDeclsNoInit(ty, vs, loc) ->
          let vs' = List.filter (fun i -> IdentSet.mem i used) vs in
          (match vs' with
          | [] -> M.unit
          | _ -> include_stmt (Stmt_VarDeclsNoInit(ty, vs', loc)))
      | Stmt_VarDecl(ty, v, i, loc) ->
          if IdentSet.mem v used
            then include_stmt stmt
            else M.unit
      | Stmt_ConstDecl(ty, v, i, loc) ->
          if IdentSet.mem v used
            then include_stmt stmt
            else M.unit
      | Stmt_Assign(LExpr_Var(v), r, loc) ->
          if IdentSet.mem v used
            then include_stmt stmt
            else M.unit
      | Stmt_If(c, tstmts, elsif, fstmts, loc) ->
          let* (_, tstmts') = M.locally_ (remove_unused' tstmts) in
          let* (_, fstmts') = M.locally_ (remove_unused' tstmts) in
          let* elsif' = M.traverse
            (fun (S_Elsif_Cond (c,ss)) ->
              let+ (_,ss') = M.locally_ (remove_unused' ss) in
              S_Elsif_Cond(c,ss')) elsif in
          (match tstmts',elsif',fstmts' with
          | [], [], [] -> M.unit
          | _ -> include_stmt (Stmt_If(c, tstmts', elsif', fstmts', loc)))
      | _ -> include_stmt stmt
    ) (List.rev xs)

  let go xs =
    let ((), _, xs') = remove_unused' xs () IdentSet.empty in
    List.rev xs'

end