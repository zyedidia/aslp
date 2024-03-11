open Asl_ast
open Symbolic

(*
  Convert an ASL decoder/instruction construct into an executable ASL program.
  The results consits of:
    - A decoder function
    - A series of instruction encoding test functions, to sanity check the result
    - A series of instruction encoding behaviour functions, corresponding to the instruction execution

  All of these functions consume the 32bit instruction encoding, with only the tests
  returning a boolean result.
*)

let enc = Ident("enc")
let enc_type = Type_Bits (expr_of_int 32)

let enc_expr opcode =
  match opcode with
  | Opcode_Bits b -> Expr_In (Expr_Var enc, Pat_LitBits b)
  | Opcode_Mask m -> Expr_In (Expr_Var enc, Pat_LitMask m)

let enc_slice lo wd = 
  Expr_Slices (Expr_Var enc, [Slice_LoWd (expr_of_int lo, expr_of_int wd)])

let field_extract loc (IField_Field (f, lo, wd)) =
  Stmt_ConstDecl (Type_Bits (expr_of_int wd), f, enc_slice lo wd, loc)

let unpred_test loc (i, b) =
  Stmt_Assert (Expr_TApply (FIdent ("ne_bits", 0), [expr_of_int 1], [enc_slice i 1; Expr_LitBits b]), loc)

let not_expr a = Expr_TApply (FIdent ("not_bool", 0), [], [a])

let decode_slice_expr s =
  match s with
  | DecoderSlice_Slice(lo, wd) -> enc_slice lo wd
  | DecoderSlice_FieldName f   -> Expr_Var f
  | DecoderSlice_Concat fs     -> failwith "DecoderSlice_Concat not expected"

let rec decode_pattern_expr p e =
  match p with
  | DecoderPattern_Bits b     -> Expr_In (e,Pat_LitBits b)
  | DecoderPattern_Mask b     -> Expr_In (e,Pat_LitMask b)
  | DecoderPattern_Wildcard _ -> expr_true
  | DecoderPattern_Not p      -> not_expr (decode_pattern_expr p e)

let get_test_fn nm = FIdent (pprint_ident nm ^ "_decode_test", 0)
let build_test_fn ((Encoding_Block (nm, _, fields, opcode, guard, unpreds, b, loc)),opost,cond,exec) =
  (* Sanity test the opcode *)
  let stmts = [Stmt_If(enc_expr opcode, [], [], [Stmt_FunReturn(expr_false, loc)], loc)] in
  (* Extract all of the instructions fields *)
  let stmts = stmts @ List.map (field_extract loc) fields in
  (* Run the encoding guard *)
  let stmts = stmts @ [Stmt_If (guard, [], [], [Stmt_FunReturn(expr_false, loc)], loc)] in
  (* Assert no unpredictable bits *)
  let stmts = stmts @ List.map (unpred_test loc) unpreds in
  (* Return true otherwise *)
  let stmts = stmts @ [Stmt_FunReturn(expr_true, loc)] in
  (* Build the function decl *)
  let fid = get_test_fn nm in
  (fid, (Some type_bool, [enc_type, enc], [], [enc], loc, stmts))

let get_body_fn nm = FIdent (pprint_ident nm, 0)
let build_instr_fn ((Encoding_Block (nm, _, fields, opcode, guard, unpreds, b, loc)),opost,cond,exec) =
  (* Extract all of the instructions fields *)
  let stmts = List.map (field_extract loc) fields in
  (* Add encoding body *)
  let stmts = stmts @ b in
  (* Add post encoding body *)
  let stmts = stmts @ (match opost with Some b -> b | _ -> []) in
  (* Add execution body *)
  let stmts = stmts @ exec in
  (* Build the function decl *)
  let fid = get_body_fn nm in
  (fid, (None, [enc_type, enc], [], [enc], loc, stmts))

let rec and_all = function
  | [e] -> e
  | e::es -> Expr_TApply (FIdent ("and_bool", 0), [], [e;and_all es])
  | [] -> expr_true

let rec decode_case vs (DecoderAlt_Alt (ps, b)) =
  let ps = List.map2 decode_pattern_expr ps vs in
  let (body, oc) = (match b with
  | DecoderBody_UNPRED loc ->  ([Stmt_Dep_Unpred(loc)], [])
  | DecoderBody_UNALLOC loc -> ([Stmt_Undefined(loc)], [])
  | DecoderBody_NOP loc -> ([], [])
  | DecoderBody_Encoding(nm, loc) -> 
      let test_fn = get_test_fn nm in
      let body_fn = get_body_fn nm in
      let test = Expr_TApply (test_fn, [], [Expr_Var enc]) in
      ([Stmt_TCall(body_fn, [], [Expr_Var enc], loc)], [test])
  | DecoderBody_Decoder (fs, c, loc) ->
      let stmts = List.map (field_extract loc) fs in
      (stmts @ build_decoder_case c, [])) in
  let c = and_all (ps @ oc) in
  S_Elsif_Cond(c, body)

and build_decoder_case (DecoderCase_Case(ss, alts, loc)) =
  let decode_slices = List.map decode_slice_expr ss in
  match List.map (decode_case decode_slices) alts with
  | S_Elsif_Cond(c,body)::xs -> [Stmt_If(c, body, xs, [Stmt_Assert(expr_false,loc)], loc)]
  | _ -> failwith "Empty decoder case"

let build_decoder iset c loc =
  let stmts = build_decoder_case c in
  let fid = FIdent(iset ^ "_decoder", 0) in
  (fid, (None, [enc_type, enc], [], [enc], loc, stmts))

let run iset pat env problematic =
  let loc = Unknown in

  (* Find all matching instructions, pulled from testing.ml *)
  let decoder = Eval.Env.getDecoder env (Ident iset) in
  let re = Pcre.regexp pat in
  let filterfn = function
    | ((Encoding_Block (Ident nm, Ident is, _, _, _, _, _, _)),_,_,_) ->
        is = iset && Pcre.pmatch ~rex:re nm && not (List.mem nm problematic)
    | _ -> assert false
  in
  let encs = List.filter filterfn (Eval.Env.listInstructions env) in

  (* Build the encoding functions *)
  let tests = List.map build_test_fn encs in
  let instr = List.map build_instr_fn encs in
  let dec = build_decoder iset decoder loc in

  (* Add to the environment *)
  List.iter (fun (f,s) -> Eval.Env.addFun loc env f s) tests;
  List.iter (fun (f,s) -> Eval.Env.addFun loc env f s) instr;
  List.iter (fun (f,s) -> Eval.Env.addFun loc env f s) [dec];

  (* Return the decoder, test functions and instruction behaviours *)
  (dec,tests,instr)
