open LibASL
open Asl_ast
open Asl_utils

let run (opcode: string) =
  let op = Z.of_string opcode in
  let bv = Primops.prim_cvt_int_bits (Z.of_int 32) op in
  let stmts = OfflineASL.Offline.run bv in
  List.iter (fun s -> Printf.printf "%s\n" (pp_stmt s)) stmts

let opt_instr = ref []
let options = Arg.align ([])
let usage_msg = ""

let _ =
  Arg.parse options
    (fun s -> opt_instr := (!opt_instr) @ [s])
    usage_msg

let main () = 
  List.map (fun instr -> run instr) !opt_instr

let _ = main()
