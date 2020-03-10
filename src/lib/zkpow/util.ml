open Core_kernel

module Tick = Snark_params.Tick
module Tock = Snark_params.Tock

(* let hash_tick_field f =
  Tick.Field.of_int (Tick.Field.hash f)

let hash_tock_field f =
  Tick.Field.of_int (Tick.Field.hash f) *)

let b64_encode_tick_proof p =
  let open Tick.Proof in
  let size = bin_size_t p in
  let buf = Bigstring.create size in
  ignore (bin_write_t buf ~pos:0 p);
  Base64.encode_string (Bigstring.to_string buf)

let b64_encode_tock_proof p =
  let open Tock.Proof in
  let size = bin_size_t p in
  let buf = Bigstring.create size in
  ignore (bin_write_t buf ~pos:0 p);
  Base64.encode_string (Bigstring.to_string buf)

let hex_decode_tick_field hex_str =
  let open Tick.Field in
  let buf = Hex.to_bigstring hex_str in
  bin_read_t buf ~pos_ref:(ref 0)