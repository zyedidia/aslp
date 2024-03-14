open Utils

let f_A64_decoder v_enc : unit = 
  failwith "unsupported"

let run enc =
  reset_ir ();
  f_A64_decoder enc;
  get_ir ()
