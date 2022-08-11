open LibASL

module Bindings = Asl_utils.Bindings

(* ADD_Z_P_ZZ__->[67108864,67117055 ; 137329664,137330687] *)
let parse_encoding l =
  (* Printf.printf ">%s<\n" l; *)
  match String.split_on_char '-' l with
  | [name; rest] ->
    let rest = String.sub rest 2 (String.length rest - 3) in
    let parse s = (match (String.split_on_char ',' s) with
    | [l; r] -> int_of_string l, int_of_string r
    | _ -> failwith ("invalid interval pair: " ^ s)) in
    (* Printf.printf ">%s<\n" rest; *)
    Asl_ast.Ident name, List.map parse (Str.split (Str.regexp " ; ") rest)
  | _ -> failwith ("expected single - in encoding string: " ^ l)

(* 0x040fffff (68157439): ADD_Z_P_ZZ__->[67108864,67117055], ASRD_Z_P_ZI__->[67403776,67411967] *)
let parse_line l =
  match String.split_on_char ':' l with
  | [_; l'] ->
    let l' = String.trim l' in
    if l' = "" then
      Bindings.empty
    else
      let encodings = Str.split (Str.regexp ", ") l' in
      List.fold_left (fun bs enc ->
        let (i,vs) = parse_encoding enc in
        Bindings.add i vs bs) Bindings.empty encodings
  | _ -> failwith ("expected single ':' in line: " ^ l)

let instrs_union = Bindings.union (fun _ l r -> Some (r @ l))
let instrs_concat = List.fold_left instrs_union Bindings.empty

(* processes a file in the output format of :enumerate *)
let parse_file f =
  let f = open_in f in
  let instrs = ref Bindings.empty in
  (try
    while true do
      let line = input_line f in
      instrs := instrs_union !instrs (parse_line line)
    done
  with End_of_file -> ());
  close_in_noerr f;
  !instrs

(* processes a list of files and merges their results.
   writes output to a subfolder with one file for each encoding containing its opcodes. *)
let main () =
  let files = (List.tl @@ Array.to_list Sys.argv) in
  let merged = instrs_concat @@ List.map parse_file files in

  Unix.mkdir "encodings" 0o777;
  Unix.chdir "encodings";

  let pp_pair (x,y) = Printf.sprintf "0x%08x,0x%08x" x y in
  let seq = Bindings.to_seq merged in
  Seq.iter (fun (ident, codes) ->
    let f = open_out (Asl_ast.pprint_ident ident) in
    List.iteri (fun i x ->
      Printf.fprintf f "%s\n" (pp_pair x)) codes;
    close_out f;
  ) seq

let _ = main ()
