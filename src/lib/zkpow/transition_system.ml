open Core_kernel
open Snark_params

module Hash_prefix = struct
  include Hash_prefix_states
end

module type S = sig
  open Tick

  module Update : Snarkable.S

  module State : sig
    module Hash : sig
      type t [@@deriving sexp]

      type var

      val typ : (var, t) Typ.t

      val var_to_field : var -> Field.Var.t

      val equal_var : var -> var -> (Boolean.var, _) Checked.t
    end

    module Body_hash : sig
      type t [@@deriving sexp]

      type var

      val typ : (var, t) Typ.t

      val var_to_field : var -> Field.Var.t
    end

    type var

    type value [@@deriving sexp]

    val typ : (var, value) Typ.t

    module Checked : sig
      val hash : var -> (Hash.var * Body_hash.var, _) Checked.t

      val is_base_case : var -> (Boolean.var, _) Checked.t

      val update :
        logger:Logger.t
        -> Hash.var * Body_hash.var * var
        (*Previous state hash, previous state body hash, previous state*)
        -> Update.var
        -> (Hash.var * var * [`Success of Boolean.var], _) Checked.t
    end
  end
end

module type Tick_keypair_intf = sig
  val keys : Tick.Keypair.t
end

module type Tock_keypair_intf = sig
  val keys : Tock.Keypair.t
end

let step_input () = Tick.Data_spec.[Tick.Field.typ]

let step_input_size = Tick.Data_spec.size (step_input ())

module State = struct
  let typ = Tick.Field.typ
  module Hash = struct
    let var_to_field f = f
  end

  module Checked = struct
    let is_base_case f =
      Tick0.Field.Checked.equal f (Tick0.Field.Var.constant Tick0.Field.zero)
  end
end

module Make (Digest : sig
    module Tick :
      Tick.Snarkable.Bits.Lossy
      with type Packed.var = Tick.Field.Var.t
       and type Packed.value = Tick.Pedersen.Digest.t
  end) =
struct
  module Step_base = struct
    (* open System *)

    module Prover_state = struct
      type t =
        { wrap_vk: Tock.Verification_key.t
        ; prev_proof: Tock.Proof.t
        ; prev_state: Tick.Field.t
        ; prev_hash: Tick.Field.t
        (* ; genesis_state_hash: Tick.Field.t *)
        ; expected_next_state: Tick.Field.t}
      [@@deriving fields]
    end

    open Tick
    open Let_syntax

    let input = step_input

    let wrap_vk_length = 11324

    let wrap_vk_typ = Typ.list ~length:wrap_vk_length Boolean.typ

    module Verifier = Tick.Verifier

    let wrap_input_size = Tock.Data_spec.size [Wrap_input.typ]
  
  
    (* let wrap_vk_triple_length =
      Verifier.Verification_key.summary_length_in_bits
        ~twist_extension_degree:3 ~input_size:wrap_input_size
      |> Util.bit_length_to_triple_length *)

    let hash_vk vk =
      make_checked (fun () ->
          Random_oracle.Checked.update
            ~state:
              (Random_oracle.State.map Hash_prefix.transition_system_snark
                 ~f:Snark_params.Tick.Field.Var.constant)
            (Verifier.Verification_key.to_field_elements vk) )

    let compute_top_hash wrap_vk_state state_hash =
      make_checked (fun () ->
          Random_oracle.Checked.(
            update ~state:wrap_vk_state [|State.Hash.var_to_field state_hash|]
            |> digest) )

    let%snarkydef prev_state_valid wrap_vk_section wrap_vk prev_state_hash =
      match Coda_compile_config.proof_level with
      | "full" ->
          (* TODO: Should build compositionally on the prev_state hash (instead of converting to bits) *)
          let%bind prev_top_hash =
            compute_top_hash wrap_vk_section prev_state_hash
            >>= Wrap_input.Checked.tick_field_to_scalars
          in
          let%bind precomp =
            Verifier.Verification_key.Precomputation.create wrap_vk
          in
          let%bind proof =
            exists Verifier.Proof.typ
              ~compute:
                As_prover.(
                  map get_state
                    ~f:
                      (Fn.compose Verifier.proof_of_backend_proof
                         Prover_state.prev_proof))
          in
          (* true if not with_snark *)
          Verifier.verify wrap_vk precomp prev_top_hash proof
      | "check" | "none" ->
          return Boolean.true_
      | _ ->
          failwith "unknown proof_level"

    let exists' typ ~f = exists typ ~compute:As_prover.(map get_state ~f)

    let%snarkydef main (next_state_hash : Digest.Tick.Packed.var) =
      (* Calculate next state and compare with given next_state_hash *)
      let%bind prev_state = exists' Field.typ ~f:Prover_state.prev_state in
      let%bind prev_state_hash = exists' Field.typ ~f:Prover_state.prev_hash in
      let%bind expected_next_state = exists' Field.typ ~f:Prover_state.expected_next_state in
      let next_state = Tick0.Field.Var.add (Tick0.Field.Var.constant Tick0.Field.one) prev_state in
      let%bind () =
        Field.Checked.Assert.(equal expected_next_state next_state)
      in
      let%bind wrap_vk =
        exists' (Verifier.Verification_key.typ ~input_size:wrap_input_size) ~f:(fun {Prover_state.wrap_vk; _} -> Verifier.vk_of_backend_vk wrap_vk )
      in
      let%bind wrap_vk_section = hash_vk wrap_vk in
      (* let%bind next_top_hash =
        with_label __LOC__
          ((* We could be reusing the intermediate state of the hash on sh here instead of
               hashing anew *)
           compute_top_hash wrap_vk_section next_state_hash)
      in *)
      let%bind () =
      Field.Checked.Assert.(equal next_state_hash next_state_hash)
      (* Field.Checked.Assert.(equal next_state_hash next_top_hash) *)
      in
      let%bind prev_state_valid =
        prev_state_valid wrap_vk_section wrap_vk prev_state_hash;
      in
      let%bind is_base_case = State.Checked.is_base_case next_state in
      with_label __LOC__
        (Boolean.Assert.any [is_base_case; prev_state_valid])


    let create_keys () = generate_keypair main ~exposing:(step_input ())
  end

  module Step (Tick_keypair : Tick_keypair_intf) = struct
    include Step_base
    include Tick_keypair
  end

  (* module Step  = struct
    include Step_base
    (* include Tick_keypair *)
  end *)

  module type Step_vk_intf = sig
    val verification_key : Tick.Verification_key.t
  end

  module Wrap_base (Step_vk : Step_vk_intf) = struct
    open Tock

    let input = Tock.Data_spec.[Wrap_input.typ]

    module Verifier = Tock.Groth_verifier

    module Prover_state = struct
      type t = {proof: Tick.Proof.t} [@@deriving fields]
    end

    let step_vk = Verifier.vk_of_backend_vk Step_vk.verification_key
    let step_vk_precomp =
      Verifier.Verification_key.Precomputation.create_constant step_vk

    let step_vk_constant = Verifier.constant_vk step_vk
    let  main (input : Wrap_input.var) =
      let%bind result =
        (* The use of choose_preimage here is justified since we feed it to the verifier, which doesn't
            depend on which unpacking is provided. *)
        let%bind input = Wrap_input.Checked.to_scalar input in
        let%bind proof =
          exists Verifier.Proof.typ
            ~compute:
              As_prover.(
                map get_state
                  ~f:
                    (Fn.compose Verifier.proof_of_backend_proof
                       Prover_state.proof))
        in
        Verifier.verify step_vk_constant step_vk_precomp [input] proof
      in
      with_label __LOC__ (Boolean.Assert.is_true result)
 
    let create_keys () = generate_keypair main ~exposing:input
  end

  module Wrap (Step_vk : Step_vk_intf) (Tock_keypair : Tock_keypair_intf) =
  struct
    include Wrap_base (Step_vk)
    include Tock_keypair
  end
end