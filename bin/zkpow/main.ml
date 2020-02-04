open Core_kernel
(* open Zkpow *)
open Snark_params

(* module Universe = (val Snarky_universe.create Tick0 Snarky_universe.Groth16) *)

(* let () = printf "Verified tick? %b\n" Step.Tick.is_valid *)
(* let () = printf "Verified tock? %b\n" Wrap.Tock.is_valid *)



let generate_genesis () = 
  let genesis_state = Tick.Field.one in
  let genesis_hash = Tick.Field.of_int (Tick.Field.hash genesis_state) in
  (genesis_state, genesis_hash)
(* 
let generate_genesis_proof state hash =
  let open Snark_params in
  let step_kp = Tick.Keypair.create () in
  Tick.prove (Tick.Keypair.pk ) *)

(* let b64_encode_hash h =
  let size = bin_size_t h in
  let buf = Bigstring.create size in
  ignore (bin_write_t buf ~pos:0 h);
  Base64.encode_string (Bigstring.to_string buf) *)

let () = 
  let genesis_state, genesis_hash = generate_genesis() in
  printf "Genesis state: %s\nGenesis hash: %s" (Tick.Field.to_string genesis_state) (Tick.Field.to_string genesis_hash)

(* let () = 
  base_hash, base_proof = generate_base()
  print "Base hash: %s" base_hash
  print "Base proof: %s" base_proof *)