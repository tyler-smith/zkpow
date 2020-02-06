open Core_kernel
open Zkpow
open Snark_params
open Util

open Transition.Make

module Keys = struct
  let step_kp = Step.create_keys ()
  let step_pk = Tick.Keypair.pk step_kp
  let step_vk = Tick.Keypair.vk step_kp

  module Wrap = Wrap (struct
    let verification_key = step_vk
  end)

  let wrap_kp = Wrap.create_keys ()
  let wrap_pk = Tock.Keypair.pk wrap_kp
  let wrap_vk = Tock.Keypair.vk wrap_kp
end
open Keys

let tick_input () =
  let open Tick in
  Data_spec.[Field.typ]

let tock_input () =
  let open Tock in
  Data_spec.[Snark_params.Wrap_input.typ]

let create_step_prover_state prev_state prev_state_hash prev_state_proof state wrap_vk=
  { Step.Prover_state.prev_proof= prev_state_proof
  ; wrap_vk= wrap_vk
  ; prev_state= prev_state
  ; prev_hash= prev_state_hash
  ; expected_next_state= state}

let create_wrap_prover_state proof =
  {Wrap.Prover_state.proof=proof}

let next_step_prover_state (state : Transition.Make.Step_base.Prover_state.t) proof =
  let open Tick in
  let expected_next_state = Field.add Field.one state.expected_next_state in
  { Step.Prover_state.prev_proof=proof
  ; wrap_vk= wrap_vk
  ; prev_state= state.expected_next_state
  ; prev_hash= (hash_tick_field state.expected_next_state)
  ; expected_next_state= expected_next_state}
  
let generate_step_proof step_pk state state_hash =
  let open Transition.Make in
  let open Tick in
  prove step_pk (tick_input ()) state Step.main2 state_hash

let generate_wrap_proof hash proof =
  let input = Wrap_input.of_tick_field hash in
  let proof =
    Tock.prove
      (Tock.Keypair.pk wrap_kp)
      Wrap.input {Wrap.Prover_state.proof} Wrap.main input
  in
  proof

let () = 
  let open Tick.Field in

  let state = create_step_prover_state zero (of_int (hash zero)) Tock.Proof.dummy one (Tock.Keypair.vk wrap_kp) in
  let proof = generate_step_proof step_pk state (hash_tick_field state.expected_next_state) in
  printf "Block0 state: %s\nBlock0 hash: %s\nBlock0 proof: %s\n" (to_string state.expected_next_state) (to_string state.prev_hash) (b64_encode_tick_proof proof);
  assert (Tick.verify proof step_vk (Step.input ()) (hash_tick_field state.expected_next_state));

  let wrap_proof = generate_wrap_proof (hash_tick_field state.expected_next_state) proof in
  printf "Block0 wrap proof: %s\n\n\n" (b64_encode_tock_proof wrap_proof);
  assert (Tock.verify wrap_proof wrap_vk Wrap.input (Wrap_input.of_tick_field(hash_tick_field state.expected_next_state)));

  (* let state = next_step_prover_state state wrap_proof in
  let proof = generate_step_proof step_pk state (hash_tick_field state.expected_next_state) in
  printf "Block1 state: %s\nBlock1 hash: %s\nBlock1 proof: %s\n" (to_string state.expected_next_state) (to_string state.prev_hash) (b64_encode_tick_proof proof);
  assert (Tick.verify proof step_vk (Step.input ()) (hash_tick_field state.expected_next_state));

  let wrap_proof = generate_wrap_proof (hash_tick_field state.expected_next_state) proof in
  printf "Block1 wrap proof: %s\n\n\n" (b64_encode_tock_proof wrap_proof);
  assert (Tock.verify wrap_proof wrap_vk Wrap.input (Wrap_input.of_tick_field(hash_tick_field state.expected_next_state))); *)

  for i = 1 to 3 do
    let state = next_step_prover_state state wrap_proof in
    let proof = generate_step_proof step_pk state (hash_tick_field state.expected_next_state) in
    printf "Block%d state: %s\nBlock%d hash: %s\nBlock%d proof: %s\n" i (to_string state.expected_next_state) i (to_string state.prev_hash) i (b64_encode_tick_proof proof);
    assert (Tick.verify proof step_vk (Step.input ()) (hash_tick_field state.expected_next_state));
  
    let wrap_proof = generate_wrap_proof (hash_tick_field state.expected_next_state) proof in
    printf "Block%d wrap proof: %s\n\n\n" i (b64_encode_tock_proof wrap_proof);
    assert (Tock.verify wrap_proof wrap_vk Wrap.input (Wrap_input.of_tick_field(hash_tick_field state.expected_next_state)));
  done


  (* let state = next_prover_state state proof in
  let proof = generate_step_proof step_pk state state.prev_hash in
  printf "Block0 state: %s\nBlock0 hash: %s\nBlock0 proof: %s\n" (to_string state.expected_next_state) (to_string state.prev_hash) (b64_encode_proof proof); *)

(* let () = 
  base_hash, base_proof = generate_base()
  print "Base hash: %s" base_hash
  print "Base proof: %s" base_proof *)