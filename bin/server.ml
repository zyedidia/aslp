open LibASL

open String
open List
open Array 
open Asl_ast
open Value
open Eval
open Asl_utils
open Lwt

let file_load_order =  ["mra_tools/arch/regs.asl"; "mra_tools/types.asl"; "mra_tools/arch/arch.asl"; "mra_tools/arch/arch_instrs.asl"; 
        "mra_tools/arch/arch_decode.asl"; "mra_tools/support/aes.asl";"mra_tools/support/barriers.asl";"mra_tools/support/debug.asl"; 
        "mra_tools/support/feature.asl"; "mra_tools/support/hints.asl";"mra_tools/support/interrupts.asl"; "mra_tools/support/memory.asl"; 
        "mra_tools/support/stubs.asl"; "mra_tools/support/fetchdecode.asl"; "tests/override.asl" ]


let default_asl_files : string = match (Res.Sites.aslfiles) with 
    | hd :: _ -> hd
    | _ -> failwith "unable to load bundled asl files."

let envinfo : Asl_ast.declaration list =
  let prelude : Asl_ast.declaration list = LoadASL.read_file (Filename.concat default_asl_files  "prelude.asl") true false     in
  let mra : Asl_ast.declaration list  list   = List.map (fun t -> LoadASL.read_file (Filename.concat default_asl_files t)  false false) file_load_order in
  List.concat (prelude :: mra)


let persistent_env     = Eval.build_evaluation_environment envinfo

let eval_instr (opcode: string) : string = 
    let praw a : string = Utils.to_string (Asl_parser_pp.pp_raw_stmt a) |> String.trim  in
    let address = None                                     in
    let res :Asl_ast.stmt list  = Dis.retrieveDisassembly ?address persistent_env (Dis.build_env persistent_env) opcode in 
    let ascii   = List.map praw res                                                 in
    let indiv (s: string) = List.init (String.length s) (String.get s) |> List.map (String.make 1)  in
    List.map indiv ascii |>  List.map (String.concat "") |> String.concat "\n"


let get_resp (opcode: string) : string = 
    match eval_instr opcode with 
        | exception e -> Printexc.to_string e
        | res -> res

let rec handle_connection ic oc () =
    Lwt_io.read_line_opt ic >>=
    (fun msg ->
        match msg with
        | Some msg -> 
            let reply = get_resp msg in
            Lwt_io.write_line oc reply >>= handle_connection ic oc
        | None -> return (print_endline "closed" ))

let create_socket (addr: string) : Lwt_unix.file_descr Lwt.t =
    let backlog = 1 in
    let open Lwt_unix in
    let sock = socket PF_UNIX SOCK_STREAM 0 in
    ((Lwt_unix.bind sock (ADDR_UNIX addr) >>= (fun f -> return (listen sock backlog) ))) >>= (fun () -> return sock)

let accept_connection conn  =
    let fd, _ = conn in
    let ic = Lwt_io.of_fd Lwt_io.Input fd in
    let oc = Lwt_io.of_fd Lwt_io.Output fd in
    Lwt.on_failure (handle_connection ic oc ()) (fun e ->  (print_endline (Printexc.to_string e)));
    (fun f -> Lwt_io.printlf "New connection") |> async; 
    return ()


let create_server sock =
    print_endline "starting server" ;
    let rec serve () = 
        sock >>= Lwt_unix.accept >>= accept_connection >>= serve in
    serve

let () = 
    let sockname = "/tmp/beans" in
    let sock = (create_socket sockname) in
    let delete_sockfile = sock >|= (fun s -> ((
        Lwt_unix.on_signal Sys.sigint (fun _ -> (print_endline "goodbye"; Lwt_unix.unlink sockname; failwith "goodbye" |> ignore))
        ) |> ignore)) in
    let serve = (create_server sock) in 
    Lwt_main.run @@ (delete_sockfile >>= (serve))
