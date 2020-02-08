(* FROM CODA util.ml *)
open Core_kernel

let bit_length_to_triple_length n =
  let r = n mod 3 in
  let k = n / 3 in
  if r = 0 then k else k + 1

let split_last_exn =
  let rec go acc x xs =
    match xs with [] -> (List.rev acc, x) | x' :: xs -> go (x :: acc) x' xs
  in
  function [] -> failwith "split_last: Empty list" | x :: xs -> go [] x xs

let two_to_the i = Bignum_bigint.(pow (of_int 2) (of_int i))

(* 
  END CODE FROM CODA
 *)

module Tick = Snark_params.Tick
module Tock = Snark_params.Tock

let hash_tick_field f =
  Tick.Field.of_int (Tick.Field.hash f)

let hash_tock_field f =
  Tick.Field.of_int (Tick.Field.hash f)

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