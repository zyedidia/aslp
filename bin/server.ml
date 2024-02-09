open LibASL


open Yojson 
open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open String
open List
open Array 
open Asl_ast
open Value
open Eval
open Asl_utils
open Lwt


let persistent_env     = Option.get (aarch64_evaluation_environment ()) 

let eval_instr (opcode: string) : string = 
    let praw a : string = Utils.to_string (Asl_parser_pp.pp_raw_stmt a) |> String.trim  in
    let address = None                                     in
    let res :Asl_ast.stmt list  = Dis.retrieveDisassembly ?address persistent_env (Dis.build_env persistent_env) opcode in 
    let ascii   = List.map praw res                                                 in
    let indiv (s: string) = List.init (String.length s) (String.get s) |> List.map (String.make 1)  in
    List.map indiv ascii |>  List.map (String.concat "") |> String.concat "\n"


let get_reply (jsonin: string) : string = 
  (*let json  = Yojson.Safe.from_string jsonin in *)
   match (eval_instr jsonin) with 
  | exception e ->  Yojson.Safe.to_string  (`Assoc [("instruction", `String jsonin); ("error", `String (Printexc.to_string e))])
  | x -> Yojson.Safe.to_string (`Assoc [("instruction", `String jsonin); ("semantics", `String x)] )
  | _ ->  Yojson.Safe.to_string  (`Assoc [("instruction", `String jsonin); ("error", `String "unknown")])


let unsupp_method_resp : string = (Yojson.Safe.to_string  (`Assoc [("error", `String "unsupported method.")]))
let missing_param : string = (Yojson.Safe.to_string  (`Assoc [("error", `String "missing opcode param.")]))

(*let () = ignore (List.map (fun (f: string) -> print_endline (eval_instr f)) (tl (to_list Sys.argv))) *)


let get_resp (opcode: string) : string = 
    get_reply opcode

let server addr port =
  print_endline "Started server.";
  let callback _conn req body =
    let uri = req |> Request.uri in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt.Body.to_string >|= (fun body ->
        match  (Request.meth req) with 
            | `POST -> get_resp body
            | `GET ->  (match  (Uri.get_query_param uri "opcode") with 
                | Some s ->  get_resp s
                | None -> missing_param)
            |  _ ->  unsupp_method_resp
        )
    >>= fun body -> Server.respond_string ~status:`OK ~body ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let port_opt : int ref = ref 8000
let addr_opt : string ref = ref "127.0.0.1"

let speclist =
  [
    ("--host", Arg.Set_string addr_opt, "Server address (default loopback)");
    ("--port", Arg.Set_int port_opt, "Server port (default 8000)");
  ]

let () = Arg.parse speclist ignore "server --host localhost --port 8000"


let () = 
     let address = Unix.(ADDR_INET (inet_addr_of_string !addr_opt, !port_opt)) in
    ignore (Lwt_main.run (server !addr_opt !port_opt))

