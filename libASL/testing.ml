module AST = Asl_ast
module Env = Eval.Env

open AST
open Value
open Asl_utils

(****************************************************************
 * Opcode decoding without evaluation.
 ****************************************************************)

(* Copies of eval_decode_alt and eval_decode_case which do not evaluate their opcodes.
   These "try" decoding each opcode. *)

(** Try to evaluate an "encoding" block to the given opcode. *)
let rec try_encoding (env: Env.t) (x: encoding) (op: value): bool =
  let Encoding_Block (nm, iset, fields, opcode, guard, unpreds, b, loc) = x in
  (* todo: consider checking iset *)
  (* Printf.printf "Checking opcode match %s == %s\n" (Utils.to_string (PP.pp_opcode_value opcode)) (pp_value op); *)
  let ok = (match opcode with
  | Opcode_Bits b -> eval_eq     loc op (from_bitsLit b)
  | Opcode_Mask m -> eval_inmask loc op (from_maskLit m)
  ) in
  let trace_instruction = ref false in
  if ok then begin
      if !trace_instruction then Printf.printf "TRACE: instruction %s\n" (pprint_ident nm);
      List.iter (function (IField_Field (f, lo, wd)) ->
          let v = extract_bits' loc op lo wd in
          if !trace_instruction then Printf.printf "      %s = %s\n" (pprint_ident f) (pp_value v);
          Env.addLocalVar loc env f v
      ) fields;
      if to_bool loc (Eval.eval_expr loc env guard) then begin
          List.iter (fun (i, b) ->
              if eval_eq loc (extract_bits' loc op i 1) (from_bitsLit b) then
                  raise (Throw (loc, Exc_Unpredictable))
          ) unpreds;
          (* try_encoding: do NOT evaluate __decode block because this may
             require IMPDEF values.
             assume its conditions are verified by earlier decode case matches. *)
          (* List.iter (eval_stmt env) b; *)
          true
      end else begin
          false
      end
  end else begin
      false
  end

(** Tests whether the given opcode can be decoded by the given decode case. *)
and try_decode_case (loc: AST.l) (env: Env.t) (x: decode_case) (op: value): ident option =
    (match x with
    | DecoderCase_Case (ss, alts, loc) ->
            let vs = List.map (fun s -> Eval.eval_decode_slice loc env s op) ss in
            let rec eval alts =
                (match alts with
                | (alt :: alts') ->
                        (match try_decode_alt loc env alt vs op with
                        | Some e -> Some e
                        | None -> eval alts')
                | [] ->
                        None (*raise (EvalError (loc, "unmatched decode pattern"))*)
                )
            in
            eval alts
    )

(** Tests whether the given opcode is decodable by the given decode case alternative.  *)
and try_decode_alt (loc: AST.l) (env: Env.t) (DecoderAlt_Alt (ps, b)) (vs: value list) (op: value): ident option =
    if List.for_all2 (Eval.eval_decode_pattern loc) ps vs then
        (match b with
        | DecoderBody_UNPRED loc -> raise (Throw (loc, Exc_Unpredictable))
        | DecoderBody_UNALLOC loc -> raise (Throw (loc, Exc_Undefined))
        | DecoderBody_NOP loc -> None (* Some (Ident "__NOP") *)
        | DecoderBody_Encoding (encname, l) ->
                let (enc, opost, cond, exec) = Env.getInstruction loc env encname in
                if try_encoding env enc op then begin
                    Some encname
                end else begin
                    None
                end
        | DecoderBody_Decoder (fs, c, loc) ->
                (* let env = Env.empty in  *)
                List.iter (function (IField_Field (f, lo, wd)) ->
                    Env.addLocalVar loc env f (extract_bits' loc op lo wd)
                ) fs;
                try_decode_case loc env c op
        )
    else
        None

(****************************************************************
 * Data type for storing intervals of integers in a compressed format.
 ****************************************************************)

(** A list of intervals, where the pair (lo,hi) indicates that all
    integers x such that lo <= x <= hi are in the list. *)
type pair_list = (int * int) list

let pair_list_cons i: pair_list -> pair_list =
  function
  | (l,r)::rest when i = succ r -> (l,i)::rest
  | rest -> (i,i)::rest

let pp_pair_list =
  Utils.pp_list (fun (x,y) -> string_of_int x ^ "," ^ string_of_int y)

let rec pair_list_mem (xs: pair_list) (x: int): bool =
  match xs with
  | [] -> false
  | (l,r)::rest ->
    if l <= x && x <= r
      then true
      else pair_list_mem rest x


(****************************************************************
 * Opcode enumeration functions.
 ****************************************************************)

let enumerate_opcodes (env: Env.t) (case: decode_case) start stop fname: unit =

  let debug_interval = Int.shift_left 1 20 in
  let i = ref start in
  let j = ref 1 in

  let t0 = (Sys.time ()) in
  let nprev = ref 0 in
  let tprev = ref t0 in
  let yes = ref 0 in
  let no = ref 0 in

  let result = ref Bindings.empty in

  let f = open_out fname in

  while !i <> stop do
    let opresult =
      (try try_decode_case Unknown env case (VBits {n=32; v=Z.of_int !i})
      with Throw _ -> None) in

    (match opresult with
    | Some e ->
        result := Bindings.update e
        (function
        | None -> Some [(!i,!i)]
        | Some old -> Some (pair_list_cons !i old)) !result;
        yes := succ !yes;
    | None ->
        no := succ !no);

    if (!j = debug_interval) then begin
      let n = !yes + !no in
      let t = Sys.time () -. t0 in

      let dn = n - !nprev in
      let dt = t -. !tprev in

      Printf.printf "t: %f, 0x%08x (%d): average %f/s (+%d in %f, %f/s), valid: %d, invalid: %d\n"
        t
        !i !i (float n /. t)
        dn dt (float dn /. dt)
        !yes !no;
      Stdlib.flush stdout;

      Printf.fprintf f "0x%08x (%d): %s\n" !i !i (pp_bindings (pp_pair_list) !result);
      Stdlib.flush f;
      result := Bindings.empty;

      nprev := n;
      tprev := t;
      j := 0;
    end;
    j := succ !j;
    i := succ !i;
  done;



(****************************************************************
 * Opcode coverage testing.
 ****************************************************************)

module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end)

let pp_intmap (f: 'a -> string) (m: 'a IntMap.t): string =
  let pairs = List.map (fun (k,v) -> Printf.sprintf "%d: %s" k (f v)) (IntMap.bindings m) in
  "{ " ^ String.concat ", " pairs ^ " }"

let hex_of_int = Printf.sprintf "0x%08x"


(** A tree of possible opcodes for a given encoding. *)
type encoding_tree =

  (* A single opcode. *)
  | Op of int

  (* A field branching into different subtrees depending on the value this
     field takes. The map is keyed by this field's values and has values of subtrees. *)
  | Field of instr_field * encoding_tree IntMap.t

let rec pp_enc_tree =
  function
  | Op i -> hex_of_int i
  | Field (f, t) ->
    let f' = pp_instr_field f in
    let t' = pp_intmap pp_enc_tree t in
    "[\"" ^ f' ^ "\": " ^ t' ^ "]"

type fields = (instr_field * int) list

let rec list_of_enc_tree (t: encoding_tree): (fields * int) list =
  (* add_field (f, i) prepends the field "f" with value "i" to the given
     (fields, opcode) pair. *)
  let add_field (f: instr_field * int) ((fs,op): fields * int) = (f::fs, op) in
  match t with
  | Op x -> [[], x]
  | Field (f, t') ->
    let x = List.map (fun (k,v) ->
      let rest = list_of_enc_tree v in
      List.map (add_field (f, k)) rest
    ) (IntMap.bindings t') in
    List.concat x

let pp_enc_fields (f: fields): string =
  Utils.pp_list (fun (f,i) -> pp_instr_field f ^ "=" ^ string_of_int i) f


let pp_enc_list (encs: (fields * int) list): string =
  String.concat "\n"
    (List.map (Utils.pp_pair pp_enc_fields hex_of_int) encs)

(* Functions for manipulating opcodes as integers. *)

let set_field (IField_Field(_,lo,wd): instr_field) (op: int) (v: int) =
  (* assert that value is within bounds for given width. *)
  assert (0 <= v);
  assert (v < Int.shift_left 1 wd);

  (* mask to zero bits for the field before applying given value. *)
  let ones_wd = Int.shift_left 1 wd - 1 in
  let mask = Int.lognot (Int.shift_left ones_wd lo) in

  Int.logor (Int.logand op mask) (Int.shift_left v lo)

let int_of_opcode: opcode_value -> int =
  function
  | Opcode_Bits bits ->
    int_of_string ("0b" ^ drop_chars bits ' ')
  | Opcode_Mask mask ->
    let v = String.map (function | 'x' -> '0' | c -> c) mask in
    int_of_string ("0b" ^ drop_chars v ' ')


(* Functions for enumerating encodings with encoding_tree. *)

let field_vals_flags_only (name: string) (wd: int): int list =
  match name with
  | _ when Utils.startswith name "R" && name <> "R" -> [1]
  | _ when Utils.startswith name "imm" -> [1]
  | _ -> List.init (Int.shift_left 1 wd) (fun x -> x)

let enumerate_encoding (enc: encoding) (field_vals: string -> int -> int list): encoding_tree =
  let Encoding_Block(name, iset, fields, opcode, guard, unpreds, stmts, loc) = enc in
  let rec go op fs =
    (match fs with
    | [] -> Op op
    | (IField_Field(name,lo,wd) as f)::rest ->
      let vals = field_vals (pprint_ident name) wd in
      let pairs = List.map (fun v -> (v, go (set_field f op v) rest)) vals in
      Field(f, IntMap.of_seq (List.to_seq pairs))
  ) in
  go (int_of_opcode opcode) fields


(* Automatic step-by-step testing of a particular opcode. *)

type operror =
  | Op_EvalFail of exn
  | Op_DisFail of exn
  | Op_DisEvalFail of exn
  | Op_DisEvalNotEqual

let pp_operror: operror -> string =
  function
  | Op_EvalFail e -> "[1] Evaluation failure: " ^ Printexc.to_string e
  | Op_DisFail e -> "[2] Disassembly failure: " ^ Printexc.to_string e
  | Op_DisEvalFail e -> "[3] Dissassembled evaluation failure: " ^ Printexc.to_string e
  | Op_DisEvalNotEqual -> "[4] Evaluation results not equal"

type 'a opresult = ('a, operror) Result.t

let pp_opresult f = Result.fold ~ok:f ~error:pp_operror

let op_eval (env: Env.t) (iset: string) (op: value): Env.t opresult =
  let evalenv = Env.copy env in
  let decoder = Eval.Env.getDecoder evalenv (Ident iset) in
  try
    Eval.eval_decode_case AST.Unknown evalenv decoder op;
    Result.Ok evalenv
  with
    | e -> Result.Error (Op_EvalFail e)

let op_dis (env: Env.t) (iset: string) (op: value): stmt list opresult =
  let env = Env.copy env in
  let decoder = Eval.Env.getDecoder env (Ident iset) in
  try
    let stmts = Dis.dis_decode_entry env decoder op in
    Result.Ok stmts
  with
    | e -> Result.Error (Op_DisFail e)

let op_diseval (env: Env.t) (stmts: stmt list): Env.t opresult =
  let env = Env.copy env in
  try
    List.iter (Eval.eval_stmt env) stmts;
    Result.Ok env
  with
    | e -> Result.Error (Op_DisEvalFail e)

let op_compare ((evalenv, disenv): Env.t * Env.t): Env.t opresult =
  if Env.compare evalenv disenv then
    Result.Ok evalenv
  else
    Result.Error (Op_DisEvalNotEqual)

let op_test_opcode (env: Env.t) (iset: string) (op: int): Env.t opresult =
  let op = Value.VBits (Primops.prim_cvt_int_bits (Z.of_int 32) (Z.of_int op)) in

  let initenv = Env.copy env in
  Random.self_init ();
  let vals = (List.init 64 (fun _ -> Z.of_int64 (Random.int64 Int64.max_int))) in
  Eval.Env.initializeRegisters initenv vals;
  Env.initializeGlobals initenv;

  let (let*) = Result.bind in
  let* evalenv = op_eval initenv iset op in
  let* disstmts = op_dis env iset op in
  let* disevalenv = op_diseval initenv disstmts in
  op_compare (evalenv, disevalenv)
