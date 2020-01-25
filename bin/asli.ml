(****************************************************************
 * ASL interactive frontend
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL interactive frontend *)

open LibASL

open Asl_ast

module Parser = Asl_parser
module TC     = Tcheck
module PP     = Asl_parser_pp
module AST    = Asl_ast

open Load_asl

let opt_filenames : string list ref = ref []
let opt_print_version = ref false

let help_msg = [
    {|:? :help                       Show this help message|};
    {|:elf <file>                    Load an ELF file|};
    {|:opcode <instr-set> <int>      Decode and execute opcode|};
    {|:project <file>                Execute ASLi commands in <file>|};
    {|:q :quit                       Exit the interpreter|};
    {|:run                           Execute instructions|};
    {|:set impdef <string> = <expr>  Define implementation defined behavior|};
    {|:set +<flag>                   Set flag|};
    {|:set -<flag>                   Clear flag|};
    {|<expr>                         Execute ASL expression|};
    {|<stmt> ;                       Execute ASL statement|}
]

let flags = [
    ("trace:write", Eval.trace_write);
    ("trace:fun",   Eval.trace_funcall);
    ("trace:prim",  Eval.trace_primop);
    ("trace:instr", Eval.trace_instruction)
]

let mkLoc (fname: string) (input: string): AST.l =
    let len = String.length input in
    let start : Lexing.position = { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
    let finish: Lexing.position = { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = len } in
    AST.Range (start, finish)

let rec process_command (tcenv: TC.Env.t) (env: Eval.Env.t) (fname: string) (input0: string): unit =
    let input = String.trim input0 in
    (match String.split_on_char ' ' input with
    | [""] ->
        ()
    | [":elf"; file] ->
        let write_byte (addr: Int64.t) (b: char): unit =
            if false then Printf.printf "ELF %LX = 0x%x\n" addr (Char.code b);
            let a = Value.VBits (Primops.mkBits 64 (Z.of_int64 addr)) in
            let b = Value.VBits (Primops.mkBits  8 (Z.of_int (Char.code b))) in
            Eval.eval_proccall AST.Unknown env (AST.FIdent ("__ELFWriteMemory", 0)) [] [a; b]
        in
        Printf.printf "Loading ELF file %s.\n" file;
        let entry = Elf.load_file file write_byte in
        Printf.printf "Entry point = 0x%Lx\n" entry
    | [":help"] | [":?"] ->
        List.iter print_endline help_msg;
        print_endline "\nFlags:";
        List.iter (fun (nm, v) -> Printf.printf "  %s%s\n" (if !v then "+" else "-") nm) flags
    | [":opcode"; iset; opcode] ->
        (* todo: make this code more robust *)
        let op = Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) (Z.of_int (int_of_string opcode))) in
        Printf.printf "Decoding and executing instruction %s %s\n" iset (Value.pp_value op);
        let decoder = Eval.Env.getDecoder env (Ident iset) in
        Eval.eval_decode_case AST.Unknown env decoder op
    | (":set" :: "impdef" :: rest) ->
        let cmd = String.concat " " rest in
        let loc = mkLoc fname cmd in
        let (x, e) = read_impdef tcenv loc cmd in
        let v = Eval.eval_expr loc env e in
        Eval.Env.setImpdef env x v
    | [":set"; flag] when Utils.startswith flag "+" ->
        (match List.assoc_opt (Utils.stringDrop 1 flag) flags with
        | None -> Printf.printf "Unknown flag %s\n" flag;
        | Some f -> f := true
        )
    | [":set"; flag] when Utils.startswith flag "-" ->
        (match List.assoc_opt (Utils.stringDrop 1 flag) flags with
        | None -> Printf.printf "Unknown flag %s\n" flag;
        | Some f -> f := false
        )
    | [":project"; prj] ->
        let inchan = open_in prj in
        (try
            while true do
                process_command tcenv env prj (input_line inchan)
            done
        with
        | End_of_file ->
            close_in inchan
        )
    | [":q"] | [":quit"] ->
        exit 0
    | [":run"] ->
        (try
            while true do
                Eval.eval_proccall AST.Unknown env (AST.FIdent ("__InstructionExecute", 0)) [] [];
            done
        with
        | Value.Throw (_, Primops.Exc_ExceptionTaken) ->
            Printf.printf "Exception taken\n"
        )
    | _ ->
        if ';' = String.get input (String.length input - 1) then begin
            let s = read_stmt tcenv input in
            Eval.eval_stmt env s
        end else begin
            let loc = mkLoc fname input in
            let e   = read_expr tcenv loc input in
            let v   = Eval.eval_expr loc env e in
            print_endline (Value.pp_value v)
        end
    )

let try_process_command (tcenv: TC.Env.t) (env: Eval.Env.t) (fname: string) (input: string): unit =
    (try
        process_command tcenv env fname input
    with
    | Parse_error_locn(l, s) -> begin
        Printf.printf "  Syntax error %s at %s\n" s (pp_loc l)
    end
    | PrecedenceError(loc, op1, op2) -> begin
        Printf.printf "  Syntax error: operators %s and %s require parentheses to disambiguate expression at location %s\n"
            (Utils.to_string (PP.pp_binop op1))
            (Utils.to_string (PP.pp_binop op2))
            (pp_loc loc)
    end
    | Parser.Error ->
        Printf.printf "  Parser error\n";
    | TC.UnknownObject (loc, what, x) ->
        Printf.printf "  %s: Type error: Unknown %s %s\n" (pp_loc loc) what x
    | TC.DoesNotMatch (loc, what, x, y) ->
        Printf.printf "  %s: Type error: %s %s does not match %s\n" (pp_loc loc) what x y
    | TC.IsNotA (loc, what, x) ->
        Printf.printf "  %s: Type error: %s is not a %s\n" (pp_loc loc) x what
    | TC.Ambiguous (loc, what, x) ->
        Printf.printf "  %s: Type error: %s %s is ambiguous\n" (pp_loc loc) what x
    | TC.TypeError (loc, what) ->
        Printf.printf "  %s: Type error: %s\n" (pp_loc loc) what
    | Value.EvalError (loc, msg) ->
        Printf.printf "  %s: Evaluation error: %s\n" (pp_loc loc) msg
    | exc ->
        Printf.printf "  Error %s\n" (Printexc.to_string exc);
        Printexc.print_backtrace stdout
    )

let rec repl (tcenv: TC.Env.t) (env: Eval.Env.t): unit =
    flush stdout;
    (match LNoise.linenoise "ASLi> " with
    | None -> ()
    | Some input ->
        LNoise.history_add input |> ignore;
        try_process_command tcenv env "<stdin>" input;
        repl tcenv env
    )

let options = Arg.align ([
    ( "-v", Arg.Set opt_verbose,              "       Verbose output");
    ( "--version", Arg.Set opt_print_version, "       Print version");
] )

let version = "ASL 0.1 alpha"

let banner = [
    {|            _____  _       _    ___________________________________|};
    {|    /\     / ____|| |     (_)   ASL interpreter                    |};
    {|   /  \   | (___  | |      _    Copyright Arm Limited (c) 2017-2019|};
    {|  / /\ \   \___ \ | |     | |                                      |};
    {| / ____ \  ____) || |____ | |   |} ^ version;
    {|/_/    \_\|_____/ |______||_|   ___________________________________|}
]
let usage_msg =
    ( version
    ^ "\nusage: asl <options> <file1> ... <fileN>\n"
    )

let _ =
  Arg.parse options
    (fun s -> opt_filenames := (!opt_filenames) @ [s])
    usage_msg

let main () =
    if !opt_print_version then Printf.printf "%s\n" version
    else begin
        List.iter print_endline banner;
        print_endline "\nType :? for help";
        let t  = read_file "prelude.asl" true in
        let ts = List.map (fun filename ->
            if Utils.endswith filename ".spec" then begin
                read_spec filename
            end else if Utils.endswith filename ".asl" then begin
                read_file filename false
            end else begin
                failwith ("Unrecognized file suffix on "^filename)
            end
        ) !opt_filenames
        in

        if !opt_verbose then Printf.printf "Building evaluation environment\n";
        let env = (try
            Eval.build_evaluation_environment (List.concat (t::ts))
        with
        | Value.EvalError (loc, msg) ->
            Printf.printf "  %s: Evaluation error: %s\n" (pp_loc loc) msg;
            exit 1
        ) in
        if !opt_verbose then Printf.printf "Built evaluation environment\n";

        LNoise.history_load ~filename:"asl_history" |> ignore;
        LNoise.history_set ~max_length:100 |> ignore;
        repl (TC.Env.mkEnv TC.env0) env
    end

let _ =ignore(main ())

(****************************************************************
 * End
 ****************************************************************)
