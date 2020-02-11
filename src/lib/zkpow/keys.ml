open Snark_params


module Protocol_state = struct
  open Snark_params.Tick

  type value = Field.t
end

module type S = sig
  module Step_prover_state : sig
    type t =
      { wrap_vk: Tock.Verification_key.t
      ; prev_proof: Tock.Proof.t
      ; prev_state: Protocol_state.value
      ; genesis_state_hash: State_hash.t
      ; expected_next_state: Tick.Field.t }
      (* ; update: Snark_transition.value } *)
  end

  module Wrap_prover_state : sig
    type t = {proof: Tick.Proof.t}
  end

  (* val transaction_snark_keys : Transaction_snark.Keys.Verification.t *)

  module Step : sig
    val keys : Tick.Keypair.t

    val input :
         unit
      -> ('a, 'b, Tick.Field.Var.t -> 'a, Tick.Field.t -> 'b) Tick.Data_spec.t

    module Verification_key : sig
      val to_bool_list : Tock.Verification_key.t -> bool list
    end

    module Prover_state = Step_prover_state

    val instance_hash : Protocol_state.value -> Tick.Field.t

    val main : Tick.Field.Var.t -> (unit, Prover_state.t) Tick.Checked.t
  end

  module Wrap : sig
    val keys : Tock.Keypair.t

    val input :
      ('a, 'b, Wrap_input.var -> 'a, Wrap_input.t -> 'b) Tock.Data_spec.t

    module Prover_state = Wrap_prover_state

    val main : Wrap_input.var -> (unit, Prover_state.t) Tock.Checked.t
  end
end

(* let pk = Snarky_keys.proving *)

(* let bc_pk = Tick.generate_keypair ~exposing(tick) *)

(* let bc_vk = lazy (Snark_keys.blockchain_verification ()) *)

(* let create () :(module S) = *)
