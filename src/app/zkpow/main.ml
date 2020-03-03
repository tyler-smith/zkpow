open Core_kernel
open Zkpow
open Snark_params
open Util

include Transition.Base

let hash_to_field_var (h : Protocol_state.Hash.t) = 
  let h_var = Protocol_state.Hash.var_of_t h in
  Protocol_state.Hash.var_to_field h_var

let hash_to_field_const_opt (h : Protocol_state.Hash.t) = 
  Tick.Field.Var.to_constant (hash_to_field_var h)

let hash_to_field (h : Protocol_state.Hash.t) = 
  match hash_to_field_const_opt h with
  | Some f -> f
  | None -> Tick.Field.zero

let hash_to_string (h : Protocol_state.Hash.t) = 
  Tick.Field.to_string (hash_to_field h)

let prove_step state state_hash =
  let open Tick in
  let proof = prove step_pk (Step.input ()) state Step.main_with_null_logger state_hash in
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
    let state_hash = Protocol_state.hash state.expected_next_state in
    let proof = prove_step state (hash_to_field state_hash) in
    printf "Block%d state: %s\nBlock%d hash: %s\nBlock%d proof: %s\n" i (Field.to_string state.expected_next_state.height) i (hash_to_string state_hash) i (b64_encode_tick_proof proof);
  
    let wrapped_proof = wrap_proof (hash_to_field state_hash) proof in
    printf "Block%d wrap proof: %s\n\n" i (b64_encode_tock_proof wrapped_proof);

    next_proof (i+1) state wrapped_proof

let proof_chain () =
  next_proof 0 state0 Tock.Proof.dummy

let () =
  proof_chain ()