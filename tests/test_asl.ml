(****************************************************************
 * Test ASL evaluator
 *
 * Copyright Arm Limited (c) 2017-2020
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL

module TC  = Tcheck
module AST = Asl_ast

let mra_tools () = ["../../../../mra_tools/arch/regs.asl"; "../../../../mra_tools/types.asl"; "../../../../mra_tools/arch/arch.asl"; "../../../../mra_tools/arch/arch_instrs.asl"; "../../../../mra_tools/arch/arch_decode.asl"; "../../../../mra_tools/support/aes.asl"; "../../../../mra_tools/support/barriers.asl"; "../../../../mra_tools/support/debug.asl"; "../../../../mra_tools/support/feature.asl"; "../../../../mra_tools/support/hints.asl"; "../../../../mra_tools/support/interrupts.asl"; "../../../../mra_tools/support/memory.asl"; "../../../../mra_tools/support/stubs.asl"; "../../../../mra_tools/support/fetchdecode.asl"]

let format_value f v = Format.fprintf f "%s" (Value.pp_value v)
let format_bool f v = Format.fprintf f "%s" (Bool.to_string v)
let value = Alcotest.testable format_value ( = )
let check_bool = Alcotest.testable format_bool ( = )

let check_int what l r = Alcotest.check value what l (Value.VInt (Z.of_int r))

let eval tcenv env (input : string): Value.value =
    let loc = AST.Unknown in
    let e = LoadASL.read_expr tcenv loc input in
    Eval.eval_expr loc env e

let test_arith tcenv env () : unit =
    check_int "1+1 == 2" (eval tcenv env "1+1") 2;
    check_int "5 DIV 3 == 1" (eval tcenv env "5 DIV 3") 1

let test_compare env () : unit =
    (* Alcotest.check value "test" (LibASL.Value.VInt (Z.of_int 1)) (LibASL.Value.VInt (Z.of_int 1)) *)
    let initializedEnv = Eval.Env.copy env in
    Random.self_init ();
    Eval.Env.initialize initializedEnv (List.map (fun _ -> Z.of_int64 (Random.int64 Int64.max_int)) (Utils.range 0 64));
    let decoder = Eval.Env.getDecoder initializedEnv (Ident "A64") in

    (* Set up our environments *)
    let evalEnv = Eval.Env.copy initializedEnv in 
    let disEnv = Eval.Env.copy env in
    let disEvalEnv = Eval.Env.copy initializedEnv in
    
    let inchan = open_in "../../../comp/comp.txt" in
    (try
        while true do
            let opcode = input_line inchan in
            let op = Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) (Z.of_int (int_of_string opcode))) in

            (* Evaluate original instruction *)
            Eval.eval_decode_case AST.Unknown evalEnv decoder op;
            
            (* Generate and evaluate partially evaluated instruction *)
            let disStmts = Dis.dis_decode_case AST.Unknown disEnv decoder op in
            Eval.eval_stmt_case Unknown disEvalEnv decoder op disStmts;
        done
    with
    | End_of_file ->
        close_in inchan
    );

    Alcotest.check check_bool "EVAL == DIS" (Eval.Env.compare evalEnv disEvalEnv) true
    (* if Eval.Env.compare evalEnv disEvalEnv then Printf.printf "No errors detected\n" else Printf.printf "Environments not equal\n" *)

let tests : unit Alcotest.test_case list =
    let prelude = LoadASL.read_file "../../../prelude.asl" true false in
    (* TODO upload the tools to github to get the paths to work. Also the test file *)
    let mra = List.map (fun tool -> LoadASL.read_file tool false false) (mra_tools ()) in
    let tcenv   = TC.Env.mkEnv TC.env0 in
    let env     = Eval.build_evaluation_environment (List.concat (prelude::mra)) in
    [
        ("arith", `Quick, test_arith tcenv env);
        ("compare", `Quick, test_compare env)
    ]

let () = Alcotest.run "libASL" [("asl", tests)]

(****************************************************************
 * End
 ****************************************************************)
