open Asl_ast
open Asl_utils
open Asl_visitor
open Visitor

(* TODO: 
  - Decoder is way too big, reduce its where possible
  - Better simp using equiv conditions in dis
*)

(* Set of functions we do not want to analyse / inline due to their complexity *)
(* TODO: It would be better to remove these using the override file, rather than hardcoding this set *)
let unsupported_set = IdentSet.of_list [
  FIdent ("AArch64.TranslateAddress", 0);
  FIdent ("AArch64.Abort", 0);
  FIdent ("Unreachable", 0);
  FIdent ("AArch64.ExclusiveMonitorsPass", 0);
  FIdent ("AArch64.SetExclusiveMonitors", 0);
]

(* Problematic instruction encoding names, due to various disassembly issues *)
let problematic_enc = [
  (* mkBits called with negative width during disassembly *)
  "FADDV_V_P_Z__";
  "FADD_Z_P_ZS__";
  "FCPY_Z_P_I__";
  "FDUP_Z_I__";
  "FMAXNMV_V_P_Z__";
  "FMAXNM_Z_P_ZS__";

  (* Timeout in dis *)
  "PNEXT_P_P_P__";

  (* Timeout in transforms *)
  "aarch64_memory_vector_multiple_no_wb";
  "aarch64_memory_vector_multiple_post_inc";
  "aarch64_vector_arithmetic_binary_element_mul_acc_complex";

  (* Outside of model *)
  "aarch64_system_exceptions_runtime_smc";
  "aarch64_system_exceptions_debug_exception";
  "aarch64_system_exceptions_runtime_svc";
  "aarch64_vector_crypto_sm4_sm4enc";
  "aarch64_vector_crypto_sm4_sm4enckey";
  "aarch64_branch_unconditional_dret";

  (* Tuple Problems *)
  "aarch64_float_convert_int";

  (* BitTuple Problems *)
  "aarch64_vector_crypto_sha3op_sha1_hash_choose";
  "aarch64_vector_crypto_sha3op_sha1_hash_majority";
  "aarch64_vector_crypto_sha3op_sha1_hash_parity";
  "aarch64_vector_crypto_sha3op_sha256_hash";

  (* Unsorted *)
  "FMAXV_V_P_Z__";
  "FMAX_Z_P_ZS__";
  "FMINNMV_V_P_Z__";
  "FMINNM_Z_P_ZS__";
  "FMINV_V_P_Z__";
  "FMIN_Z_P_ZS__";
  "FMUL_Z_P_ZS__";
  "FSUBR_Z_P_ZS__";
  "FSUB_Z_P_ZS__";
  "LD2B_Z_P_BI_Contiguous";
  "LD2B_Z_P_BR_Contiguous";
  "LD2D_Z_P_BI_Contiguous";
  "LD2D_Z_P_BR_Contiguous";
  "LD2H_Z_P_BI_Contiguous";
  "LD2H_Z_P_BR_Contiguous";
  "LD2W_Z_P_BI_Contiguous";
  "LD2W_Z_P_BR_Contiguous";
  "LD3B_Z_P_BI_Contiguous";
  "LD3B_Z_P_BR_Contiguous";
  "LD3D_Z_P_BI_Contiguous";
  "LD3D_Z_P_BR_Contiguous";
  "LD3H_Z_P_BI_Contiguous";
  "LD3H_Z_P_BR_Contiguous";
  "LD3W_Z_P_BI_Contiguous";
  "LD3W_Z_P_BR_Contiguous";
  "LD4B_Z_P_BI_Contiguous";
  "LD4B_Z_P_BR_Contiguous";
  "LD4D_Z_P_BI_Contiguous";
  "LD4D_Z_P_BR_Contiguous";
  "LD4H_Z_P_BI_Contiguous";
  "LD4H_Z_P_BR_Contiguous";
  "LD4W_Z_P_BI_Contiguous";
  "LD4W_Z_P_BR_Contiguous";
  "ST2B_Z_P_BI_Contiguous";
  "ST2B_Z_P_BR_Contiguous";
  "ST2D_Z_P_BI_Contiguous";
  "ST2D_Z_P_BR_Contiguous";
  "ST2H_Z_P_BI_Contiguous";
  "ST2H_Z_P_BR_Contiguous";
  "ST2W_Z_P_BI_Contiguous";
  "ST2W_Z_P_BR_Contiguous";
  "ST3B_Z_P_BI_Contiguous";
  "ST3B_Z_P_BR_Contiguous";
  "ST3D_Z_P_BI_Contiguous";
  "ST3D_Z_P_BR_Contiguous";
  "ST3H_Z_P_BI_Contiguous";
  "ST3H_Z_P_BR_Contiguous";
  "ST3W_Z_P_BI_Contiguous";
  "ST3W_Z_P_BR_Contiguous";
  "ST4B_Z_P_BI_Contiguous";
  "ST4B_Z_P_BR_Contiguous";
  "ST4D_Z_P_BI_Contiguous";
  "ST4D_Z_P_BR_Contiguous";
  "ST4H_Z_P_BI_Contiguous";
  "ST4H_Z_P_BR_Contiguous";
  "ST4W_Z_P_BI_Contiguous";
  "ST4W_Z_P_BR_Contiguous";
  "WRFFR_F_P__";
  "aarch64_integer_crc";
  "aarch64_integer_pac_pacga_dp_2src";
  "aarch64_integer_pac_strip_dp_1src";
  "aarch64_integer_pac_strip_hint";
  "aarch64_memory_vector_single_no_wb";
  "aarch64_memory_vector_single_post_inc";
  "aarch64_system_hints";
  "aarch64_system_register_cpsr";
  "aarch64_system_register_system";
  "aarch64_vector_shift_left_insert_simd";
  "aarch64_vector_shift_left_sat_sisd";
  "aarch64_vector_shift_right_insert_simd";
  "aarch64_vector_shift_right_narrow_nonuniform_sisd";
  "aarch64_vector_shift_right_narrow_uniform_sisd";
  "aarch64_vector_transfer_integer_dup";
  "aarch64_vector_transfer_integer_insert";
  "aarch64_vector_transfer_vector_cpy_dup_simd";
  "aarch64_vector_transfer_vector_cpy_dup_sisd";
  "aarch64_vector_transfer_vector_insert";
  "aarch64_vector_transfer_vector_table";
]

(** Trival walk to replace unsupported calls with a corresponding throw *)
module RemoveUnsupported = struct
  let assert_false loc = Stmt_Throw(Ident ("UNSUPPORTED"), loc)

  class expr_visitor unsupported = object
    inherit Asl_visitor.nopAslVisitor
    val mutable seen = false
    method! vexpr e =
      (match e with
      | Expr_TApply (f, _, _) -> 
          if unsupported f then (seen <- true; SkipChildren)
          else DoChildren
      | _ -> DoChildren)
    method has_unsupported = seen
  end 

  let contains_unsupported e unsupported =
    let v = new expr_visitor unsupported in
    let _ = visit_expr v e in
    v#has_unsupported

  class call_visitor unsupported  = object
    inherit Asl_visitor.nopAslVisitor

    method! vstmt e =
      (match e with
      | Stmt_Assert(e, loc) ->
          if contains_unsupported e unsupported then ChangeTo (assert_false loc)
          else DoChildren

      | Stmt_VarDeclsNoInit _
      | Stmt_ProcReturn _ -> DoChildren

      | Stmt_FunReturn (e, loc) ->
          if contains_unsupported e unsupported then ChangeTo (assert_false loc)
          else DoChildren

      | Stmt_VarDecl(ty, v, e, loc) ->
          if contains_unsupported e unsupported then ChangeTo (assert_false loc)
          else DoChildren

      | Stmt_ConstDecl(ty, v, e, loc) ->
          if contains_unsupported e unsupported then ChangeTo (assert_false loc)
          else DoChildren

      | Stmt_Assign(v, e, loc) ->
          if contains_unsupported e unsupported then ChangeTo (assert_false loc)
          else DoChildren

      | Stmt_TCall (f, tes, es, loc) -> 
          if unsupported f then ChangeTo (assert_false loc)
          else if List.exists (fun e -> contains_unsupported e unsupported) (tes @ es) then ChangeTo (assert_false loc)
          else DoChildren

      | Stmt_If (c, t, alts, f, loc) ->
          if contains_unsupported c unsupported then ChangeTo (assert_false loc)
          else if List.exists (fun (S_Elsif_Cond(c,_)) -> contains_unsupported c unsupported) alts then ChangeTo (assert_false loc)
          else DoChildren

      | Stmt_Case (e, alts, odefault, loc) ->
          if contains_unsupported e unsupported then ChangeTo (assert_false loc)
          else DoChildren

      | Stmt_While (c, b, loc) ->
          if contains_unsupported c unsupported then ChangeTo (assert_false loc)
          else DoChildren

      | Stmt_For(var, start, dir, stop, body, loc) ->
          if contains_unsupported start unsupported then ChangeTo (assert_false loc)
          else if contains_unsupported stop unsupported then ChangeTo (assert_false loc)
          else DoChildren

      | Stmt_Dep_Undefined loc
      | Stmt_Undefined loc 
      | Stmt_Unpred loc
      | Stmt_ConstrainedUnpred loc
      | Stmt_ImpDef (_, loc)
      | Stmt_ExceptionTaken loc
      | Stmt_Dep_Unpred loc
      | Stmt_Dep_ImpDef (_, loc)
      | Stmt_See (_, loc)
      | Stmt_Throw (_, loc)
      | Stmt_DecodeExecute (_, _, loc) -> ChangeTo (assert_false loc)

      | _ -> failwith @@ "Unknown stmt: " ^ (pp_stmt e))
  end

  let run unsupported =
    let v = new call_visitor unsupported in
    visit_stmts v

end

let dis_wrapper fn fnsig env =
  Printf.printf "  Running %s\n" (name_of_FIdent fn);
  flush stdout;
  let (lenv,globals) = Dis.build_env env in
  try
    let body = fnsig_get_body fnsig in
    let sym = Symbolic.Exp (Expr_Var (Decoder_program.enc)) in
    let (_,lenv,_) = (Dis.declare_assign_var Unknown (Type_Bits (Expr_LitInt "32")) (Ident "enc") sym) env lenv in
    let ((),lenv',stmts) = (Dis.dis_stmts body) env lenv in
    let stmts = Dis.flatten stmts [] in
    let stmts' = Transforms.RemoveUnused.remove_unused globals @@ stmts in
    let stmts' = Transforms.RedundantSlice.do_transform Bindings.empty stmts' in
    let stmts' = Transforms.StatefulIntToBits.run (Dis.enum_types env) stmts' in
    let stmts' = Transforms.IntToBits.ints_to_bits stmts' in
    (* TODO: This would be nice to run, but makes a mess right now *)
    (*let stmts' = Transforms.CommonSubExprElim.do_transform stmts' in*)
    let stmts' = Transforms.CopyProp.copyProp stmts' in
    let stmts' = Transforms.RemoveUnused.remove_unused globals @@ stmts' in 
    Some stmts'
  with 
  | e ->
    Printf.printf "Error: %s %s\n" (name_of_FIdent fn) (Printexc.to_string e);
    None

let unsupported f = IdentSet.mem f unsupported_set

let get_inlining_frontier =
  (* Collect all functions dis will not inline *)
  let l1 = IdentSet.of_list (List.map (fun (f,i) -> FIdent (f,i)) Dis.no_inline) in
  let l2 = IdentSet.of_list (List.map (fun (f,i) -> FIdent (f,i)) Dis.no_inline_pure) in
  (* Collect all prims *)
  let l3 = IdentSet.of_list (List.map (fun f -> FIdent (f,0)) Value.prims_pure) in
  let l4 = IdentSet.of_list (List.map (fun f -> FIdent (f,0)) Value.prims_impure) in
  (* Union with the unsupported function set *)
  IdentSet.union l1 (IdentSet.union l2 (IdentSet.union l3 (IdentSet.union l4 unsupported_set)))

(* Count individual stmts present after disassembly *)
let rec stmt_count s =
  match s with
  | Stmt_If (_,t,[],f,_) ->
      1 + stmts_count t + stmts_count f
  | _ -> 1
and stmts_count s =
  List.fold_right (fun s acc -> stmt_count s + acc) s 0

module Cleanup = struct

  let rec zip_pre a b =
    match a, b with
    | x::xs, y::ys when x = y ->
        let (common,xs,ys) = zip_pre xs ys in
        (x::common,xs,ys)
    | _ -> ([],a,b)

  let zip_post a b =
    let (common,a,b) = zip_pre (List.rev a) (List.rev b) in
    (List.rev common, List.rev a, List.rev b)

  let rec walk_stmt s =
    match s with
    | Stmt_If(c, t, els, f, loc) when t = f && List.for_all (function S_Elsif_Cond(_,b) -> b = t) els ->
        t

    | Stmt_If(c, t, [], f, loc) ->
      let (common_pre,t,f) = zip_pre t f in
      let (common_post,t,f) = zip_post t f in
      let common_pre = walk_stmts common_pre in
      let common_post = walk_stmts common_post in
      let t = walk_stmts t in
      let f = walk_stmts f in
      (* TODO: c & common_pre *)
      common_pre @ Stmt_If(c, t, [], f, loc) :: common_post

    | Stmt_If(c, t, els, f, loc) ->
        let t = walk_stmts t in
        let els = List.map (function S_Elsif_Cond(c,b) -> S_Elsif_Cond(c,walk_stmts b)) els in
        let f = walk_stmts f in 
        [Stmt_If(c, t, els, f, loc)]

    | _ -> [s]

  and walk_stmts s =
    List.fold_left (fun acc s -> 
      let s = walk_stmt s in
      acc@s) [] s

  let run = walk_stmts

end

module DecoderCleanup = struct

  (* Remove unsupported decode tests *)
  class expr_visitor unsupported = object
    inherit nopAslVisitor
    method! vexpr e =
      (match e with
      | Expr_TApply (f, _, _) ->
          let suffix = "_decode_test" in
          if String.ends_with ~suffix (name_of_FIdent f) && unsupported f then ChangeTo (Symbolic.expr_true)
          else DoChildren
      | _ -> DoChildren)
  end 

  (* Remove unsupported instr bodies *)
  class call_visitor unsupported  = object
    inherit nopAslVisitor
    method! vstmt e =
      (match e with
      | Stmt_TCall (f, _, _, loc) ->
          if unsupported f then ChangeTo (RemoveUnsupported.assert_false loc)
          else DoChildren
      | _ -> DoChildren)
  end

  let run unsupported dsig =
    let v = new expr_visitor unsupported in
    let dsig = (visit_stmts v) dsig in
    let v = new call_visitor unsupported in
    let dsig = (visit_stmts v) dsig in
    dsig
end

let unsupported_inst tests instrs f = 
  let r = not (List.exists (fun (f',_) -> f' = f) (tests@instrs))  in
  r

(* Produce a lifter for the desired parts of the instruction set *)
let run iset pat env =
  Printf.printf "Stage 1: Mock decoder & instruction encoding definitions\n";
  let ((did,dsig),tests,instrs) = Decoder_program.run iset pat env problematic_enc in
  let entry_set = List.fold_right (fun (k,s) -> IdentSet.add k) instrs IdentSet.empty in
  Printf.printf "  Collected %d instructions\n\n" (IdentSet.cardinal entry_set);

  Printf.printf "Stage 2: Call graph construction\n";
  let frontier = get_inlining_frontier in
  let (callers, reachable) = Call_graph.run entry_set frontier env in
  let fns = IdentSet.fold (fun id acc -> Bindings.add id (Eval.Env.getFun Unknown env id) acc) reachable Bindings.empty in
  Printf.printf "  Collected %d functions\n\n" (Bindings.cardinal fns);

  Printf.printf "Stage 3: Simplification\n\n";
  (* Remove temporary dynamic bitvectors where possible *)
  let fns = Bindings.map (fnsig_upd_body (Transforms.RemoveTempBVs.do_transform false)) fns in
  (* Remove calls to problematic functions *)
  let fns = Bindings.map (fnsig_upd_body (RemoveUnsupported.run unsupported)) fns in
  (* Prune unsupported instructions from decoder *)
  let dsig = fnsig_upd_body (DecoderCleanup.run (unsupported_inst tests instrs)) dsig in
  (* Dead code elim on decoder & tests *)
  let dsig = fnsig_upd_body (Transforms.RemoveUnused.remove_unused IdentSet.empty) dsig in
  let dsig = fnsig_upd_body (Cleanup.run) dsig in
  let tests = List.map (fun (f,s) -> (f,fnsig_upd_body (Transforms.RemoveUnused.remove_unused IdentSet.empty) s)) tests in

  Printf.printf "Stage 4: Specialisation\n";
  (* Run requirement collection over the full set *)
  let fns = Req_analysis.run fns callers in
  Printf.printf "\n";

  Printf.printf "Stage 5: Disassembly\n";
  (* Build an environment with these new function definitions *)
  let env' = Eval.Env.copy env in
  Bindings.iter (fun  fn fnsig  -> Eval.Env.addFun Unknown env' fn fnsig) fns;
  (* Run dis over the entry set identifiers with this new environment *)
  let fns = IdentSet.fold (fun fn acc ->
    let fnsig = Bindings.find fn fns in
    match dis_wrapper fn fnsig env' with
    | Some body -> Bindings.add fn (fnsig_set_body fnsig body) acc
    | None -> acc) entry_set Bindings.empty in
  Printf.printf "  Succeeded for %d instructions\n\n" (Bindings.cardinal fns);

  let fns = Bindings.map (fnsig_upd_body (Cleanup.run)) fns in

  Printf.printf "Stmt Counts\n"; 
  Bindings.iter (fun fn fnsig -> Printf.printf "  %d : %s\n" (stmts_count (fnsig_get_body fnsig)) (name_of_FIdent fn)) fns;

  ((did,dsig),tests,fns)
