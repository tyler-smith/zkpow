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

let step_prover_state_zero =
  let open Tick.Field in
  let negative_one = negate one in
  create_step_prover_state negative_one (of_int (hash negative_one)) Tock.Proof.dummy zero (Tock.Keypair.vk wrap_kp)

let next_step_prover_state (state : Transition.Make.Step_base.Prover_state.t) proof =
  let open Tick in
  let expected_next_state = Field.add Field.one state.expected_next_state in
  { Step.Prover_state.prev_proof=proof
  ; wrap_vk= wrap_vk
  ; prev_state= state.expected_next_state
  ; prev_hash= (hash_tick_field state.expected_next_state)
  ; expected_next_state= expected_next_state}
  
let create_wrap_prover_state proof =
  {Wrap.Prover_state.proof=proof}

let create_step_proof step_pk state state_hash =
  let open Transition.Make in
  let open Tick in
  let proof = prove step_pk (tick_input ()) state Step.main2 state_hash in
  assert (Tick.verify proof step_vk (Step.input ()) state_hash);
  proof

let wrap_proof hash proof =
  let input = Wrap_input.of_tick_field hash in
  let proof =
    Tock.prove
      (Tock.Keypair.pk wrap_kp)
      Wrap.input {Wrap.Prover_state.proof} Wrap.main input
  in
  assert (Tock.verify proof wrap_vk Wrap.input (Wrap_input.of_tick_field hash));
  proof


let rec create_next_proof (i:int) state wrapped_proof =
  if i >= 10 then ()
  else
    let module Field = Tick.Field in
    let state = next_step_prover_state state wrapped_proof in
    let state_hash = (hash_tick_field state.expected_next_state) in
    let proof = create_step_proof step_pk state state_hash in
    printf "Block%d state: %s\nBlock%d hash: %s\nBlock%d proof: %s\n" i (Field.to_string state.expected_next_state) i (Field.to_string state_hash) i (b64_encode_tick_proof proof);
  
    let wrapped_proof = wrap_proof state_hash proof in
    printf "Block%d wrap proof: %s\n\n" i (b64_encode_tock_proof wrapped_proof);

    create_next_proof (i+1) state wrapped_proof

let () = 
  create_next_proof 0 step_prover_state_zero Tock.Proof.dummy