open Core_kernel
open Zkpow
open Snark_params
open Util

include Transition.Base

let state0 =
  let open Tick.Field in
  { State.prev_proof= Tock.Proof.dummy
  ; wrap_vk= wrap_vk
  ; prev_state= negate one
  ; prev_hash= hash_tick_field (negate one)
  ; expected_next_state= zero}

let next_state (state : State.t) proof =
  let open Tick.Field in
  let expected_next_state = add one state.expected_next_state in
  { Step.Prover_state.prev_proof=proof
  ; wrap_vk= wrap_vk
  ; prev_state= state.expected_next_state
  ; prev_hash= (hash_tick_field state.expected_next_state)
  ; expected_next_state= expected_next_state}

let prove_step step_pk state state_hash =
  let open Tick in
  let proof = prove step_pk (Step.input ()) state Step.main state_hash in
  assert (verify proof step_vk (Step.input ()) state_hash);
  proof

let wrap_proof hash proof =
  let open Tock in
  let open Wrap in
  let wrapped_input = Wrap_input.of_tick_field hash in
  let proof =
    prove wrap_pk input {WrappedState.proof} main wrapped_input
  in
  assert (verify proof wrap_vk input wrapped_input);
  proof

let rec next_proof (i:int) state wrapped_proof =
  if i >= 3 then ()
  else
    let module Field = Tick.Field in
    let state = next_state state wrapped_proof in
    let state_hash = (hash_tick_field state.expected_next_state) in
    let proof = prove_step step_pk state state_hash in
    printf "Block%d state: %s\nBlock%d hash: %s\nBlock%d proof: %s\n" i (Field.to_string state.expected_next_state) i (Field.to_string state_hash) i (b64_encode_tick_proof proof);
  
    let wrapped_proof = wrap_proof state_hash proof in
    printf "Block%d wrap proof: %s\n\n" i (b64_encode_tock_proof wrapped_proof);

    next_proof (i+1) state wrapped_proof

let () = 
  next_proof 0 state0 Tock.Proof.dummy