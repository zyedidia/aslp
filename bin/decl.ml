open LibASL
open Unsigned.UInt32
open Domainslib

open Ctypes
open Foreign
let evaluate = foreign "evaluate" (uint32_t @-> returning bool)
let set = foreign "set" (uint32_t @-> uint32_t @-> returning void)
let write_all = foreign "write_all" (void @-> returning void)
let inc = foreign "inc" (void @-> returning void)

open Asl_ast
open Value
open Eval
open Asl_utils

module Parser = Asl_parser
module TC     = Tcheck
module PP     = Asl_parser_pp
module AST    = Asl_ast

let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let main () =
    let env_opt =
        aarch64_evaluation_environment ~verbose:false () in

    let env = (match env_opt with 
        | Some e -> e
        | None -> failwith "Unable to build evaluation environment.") in

    let denv = Dis.build_env env in
    let tcenv = TC.Env.mkEnv TC.env0 and cpu = Cpu.mkCPU env denv in

    Printf.fprintf stderr "loaded\n%!";

    let pool = Task.setup_pool ~num_domains:32 () in

    Task.run pool (fun () -> Task.parallel_for pool ~start:0 ~finish:(0xffffffff) ~body:(fun i ->
        if not (evaluate (Unsigned.UInt32.of_int i)) then begin
            set (Unsigned.UInt32.of_int i) (Unsigned.UInt32.of_int 3);
        end else begin
            (try
                LoadASL.report_eval_error (fun _ -> ()) (fun _ ->
                    LoadASL.report_type_error (fun _ -> ()) (fun _ ->
                        LoadASL.report_parse_error (fun _ -> ()) (fun _ ->
                            let op = Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) (Z.of_int i)) in
                            let decoder = Eval.Env.getDecoder cpu.env (Ident "A64") in
                            List.iter
                                (fun s ->
                                    if contains (Utils.to_string (PP.pp_raw_stmt s)) "Unpred.0" then
                                        set (Unsigned.UInt32.of_int i) (Unsigned.UInt32.of_int 1)
                                    else
                                        set (Unsigned.UInt32.of_int i) (Unsigned.UInt32.of_int 0)
                                )
                                (Dis.dis_decode_entry cpu.env cpu.denv decoder op);
                        )
                    )
                )
            with
            | exc ->
                set (Unsigned.UInt32.of_int i) (Unsigned.UInt32.of_int 2)
            );
        end;
        inc ();
    ));

    write_all ()

let _ = ignore (main ())
