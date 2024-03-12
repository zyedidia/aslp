(****************************************************************
 * CPU interface
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

open Asl_utils

type cpu = {
    env      : Eval.Env.t;
    denv     : Dis.env;
    reset    : unit -> unit;
    step     : unit -> unit;
    getPC    : unit -> Primops.bigint;
    setPC    : Primops.bigint -> unit;
    elfwrite : Int64.t -> char -> unit;
    opcode   : string -> Primops.bigint -> unit;
    sem      : string -> Primops.bigint -> unit;
    gen      : string -> string -> unit
}

let mkCPU (env : Eval.Env.t) (denv: Dis.env): cpu =
    let loc = AST.Unknown in

    let reset (_ : unit): unit =
        Eval.eval_proccall loc env (AST.FIdent ("__TakeColdReset", 0)) [] []

    and step (_ : unit): unit =
        Eval.eval_proccall loc env (AST.FIdent ("__InstructionExecute", 0)) [] []

    and getPC (_ : unit): Primops.bigint =
        let r = Eval.eval_funcall loc env (AST.FIdent ("__getPC", 0)) [] [] in
        Value.to_integer loc r

    and setPC (x : Primops.bigint): unit =
        let a = Value.VInt x in
        Eval.eval_proccall loc env (AST.FIdent ("__setPC", 0)) [] [a]

    and elfwrite (addr: Int64.t) (b: char): unit =
        let a = Value.VBits (Primops.mkBits 64 (Z.of_int64 addr)) in
        let b = Value.VBits (Primops.mkBits  8 (Z.of_int (Char.code b))) in
        Eval.eval_proccall loc env (AST.FIdent ("__ELFWriteMemory", 0)) [] [a; b]

    and opcode (iset: string) (opcode: Primops.bigint): unit =
        let op = Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) opcode) in
        let decoder = Eval.Env.getDecoder env (Ident iset) in
        Eval.eval_decode_case AST.Unknown env decoder op

    and sem (iset: string) (opcode: Primops.bigint): unit =
        let op = Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) opcode) in
        let decoder = Eval.Env.getDecoder env (Ident iset) in
        List.iter
            (fun s -> Printf.printf "%s\n" (pp_stmt s))
            (Dis.dis_decode_entry env denv decoder op)

    and gen (iset: string) (pat: string): unit =
        (* Build the symbolic lifter *)
        let (decoder,tests,fns) = Symbolic_lifter.run iset pat env in

        (* Perform offline PE *)
        let offline_fns = Offline_transform.run fns env in

        (* Add trivial offline functions *)
        let offline_fns = List.fold_right (fun (f,s) -> Bindings.add f s) tests offline_fns in
        let offline_fns = Bindings.add (fst decoder) (snd decoder) offline_fns in

        (* Dump the resulting program as OCaml *)
        if not (Sys.file_exists "output") then Sys.mkdir "output" 755;
        Ocaml_backend.run (fst decoder) offline_fns env "output/offline.ml"

    in
    {
        env      = env;
        denv     = denv;
        reset    = reset;
        step     = step;
        getPC    = getPC;
        setPC    = setPC;
        elfwrite = elfwrite;
        opcode   = opcode;
        sem      = sem;
        gen      = gen
    }

(****************************************************************
 * End
 ****************************************************************)
