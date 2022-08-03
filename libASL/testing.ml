module AST = Asl_ast
module Env = Eval.Env

open AST
open Value
open Asl_utils

let pp_eval_env (env: Eval.Env.t): string =
  let globals = (Eval.Env.readGlobals env) in
  (* let locals = Eval.Env.readLocals env in *)
  Printf.sprintf
{|Env {
  globals = %s;
  locals = %s;
}|}
  ("{ _R = " ^ (Value.pp_value (Asl_utils.Bindings.find (Ident "_R") globals)) ^ "}")
  "[...]"
  (* (Asl_utils.pp_bindings Value.pp_value globals)
  (Dis.LocalEnv.pp_value_bindings locals) *)

(* Copies of eval_decode_alt and eval_decode_case which do not evaluate their opcodes. *)

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

type pair_list = (int * int) list

let pair_list_cons i: pair_list -> pair_list =
  function
  | (l,r)::rest when i = succ r -> (l,i)::rest
  | rest -> (i,i)::rest

let pp_pair_list =
  Utils.pp_list (fun (x,y) -> string_of_int x ^ "," ^ string_of_int y)

let try_decode_all (env: Env.t) (case: decode_case): pair_list Bindings.t =

  let debug_interval = Int.shift_left 1 20 in
  let max = Int.shift_left 1 32 in
  let i = ref 0 in
  let j = ref 1 in

  let t0 = (Sys.time ()) in
  let nprev = ref 0 in
  let tprev = ref t0 in
  let yes = ref 0 in
  let no = ref 0 in

  let result = ref Bindings.empty in

  let f = open_out "ops.txt" in

  while !i <> max do
    let opresult =
      (try try_decode_case Unknown env case (VBits {n=64; v=Z.of_int !i})
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
      let n = (!yes + !no) in
      let t = Sys.time () in
      Printf.printf "t: %f (0x%08x), n: %d (+%d in %f), valid: %d, invalid: %d\n"
        (Sys.time () -. t0) n !i
        (n - !nprev) (t -. !tprev)
        !yes !no;
      Stdlib.flush stdout;

      Printf.fprintf f "%s\n" (pp_bindings (pp_pair_list) !result);
      Stdlib.flush f;
      result := Bindings.empty;

      nprev := n;
      tprev := t;
      j := 0;
    end;
    j := succ !j;
    i := succ !i;
  done;
  !result
