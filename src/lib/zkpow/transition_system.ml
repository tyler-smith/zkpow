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
    end

    type var

    type value [@@deriving sexp]

    val typ : (var, value) Typ.t

    module Checked : sig
      val hash : var -> (Hash.var, _) Checked.t

      val is_base_hash : Hash.var -> (Boolean.var, _) Checked.t

      val update :
           logger:Logger.t
        -> Hash.var * var
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

(* module State = struct
  let typ = Tick.Field.typ
  module Hash = struct
    let var_to_field f = f
  end

  module Checked = struct
    let is_base_case f =
      Tick0.Field.Checked.equal f (Tick0.Field.Var.constant Tick0.Field.zero)
  end
end *)

(* input is the public input into the circuit *)
let step_input () = Tick.Data_spec.[Tick.Field.typ]
(* let step_input () = Tick.Data_spec.[Protocol_state.Hash.typ] *)

let step_input_size = Tick.Data_spec.size (step_input ())

module Make (Digest : sig
    module Tick :
      Tick.Snarkable.Bits.Lossy
      with type Packed.var = Tick.Field.Var.t
       and type Packed.value = Tick.Pedersen.Digest.t
  end) (System : S) =
struct
  module Step_base = struct
    open System

    (* Prover_state is the private input into the circuit *)
    module Prover_state = struct
      type t =
        { wrap_vk: Tock.Verification_key.t
        ; prev_proof: Tock.Proof.t
        ; prev_state: State.value
        ; expected_next_state: State.value}
      [@@deriving fields]
    end

    open Tick
    open Let_syntax

    let input = step_input

    let wrap_vk_length = 11324

    let wrap_vk_typ = Typ.list ~length:wrap_vk_length Boolean.typ

    (* module Verifier = Tick.Verifier *)

    let wrap_input_size = Tock.Data_spec.size [Wrap_input.typ]
  
    let hash_vk vk =
      make_checked (fun () ->
          Random_oracle.Checked.update
            ~state:
              (Random_oracle.State.map Hash_prefix.transition_system_snark
                 ~f:Snark_params.Tick.Field.Var.constant)
            (Verifier.Verification_key.to_field_elements vk) )

    (* The top hash is the public input into the circuit and it commits to the state inputs and the wrap vk hash *)
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

    let inc_tick_field f =
      let open Tick0.Field in
      Var.add f (Var.constant one)

    let%snarkydef main (logger : Logger.t) (top_hash : Digest.Tick.Packed.var) =
      let%bind prev_state = exists' State.typ ~f:Prover_state.prev_state in
      (* let%bind update = exists' Update.typ ~f:Prover_state.update in *)
      let%bind prev_state_hash = State.Checked.hash prev_state in

      let%bind _next_state = exists' State.typ ~f:Prover_state.expected_next_state in
      let%bind next_state_hash = State.Checked.hash _next_state in
      let%bind success = Checked.return Boolean.true_ in
      (* let%bind next_state_hash, _next_state, `Success success =
        with_label __LOC__
          (State.Checked.update ~logger (prev_state_hash, prev_state) update)
      in *)
      let%bind wrap_vk =
        exists' (Verifier.Verification_key.typ ~input_size:wrap_input_size)
          ~f:(fun {Prover_state.wrap_vk; _} ->
            Verifier.vk_of_backend_vk wrap_vk )
      in
      let%bind wrap_vk_section = hash_vk wrap_vk in
      let%bind next_top_hash =
        with_label __LOC__
          ((* We could be reusing the intermediate state of the hash on sh here instead of
              hashing anew *)
          compute_top_hash wrap_vk_section next_state_hash)
      in
      let%bind () =
        as_prover
          As_prover.(
            Let_syntax.(
              let%bind prover_state = get_state in
              let%bind in_snark_next_state = read State.typ _next_state in
              let%bind next_top_hash = read Field.typ next_top_hash in
              let%bind top_hash = read Field.typ top_hash in
              let updated = State.sexp_of_value in_snark_next_state in
              let original = State.sexp_of_value (Prover_state.expected_next_state prover_state) in
              ( if not (Field.equal next_top_hash top_hash) then
                let diff =
                  Sexp_diff_kernel.Algo.diff ~original ~updated ()
                in
                Logger.fatal logger
                  "Out-of-SNARK and in-SNARK calculations of the next top \
                  hash differ"
                  ~metadata:
                    [ ( "state_sexp_diff"
                      , `String
                          (Sexp_diff_kernel.Display.display_as_plain_string
                            diff) ) ]
                  ~location:__LOC__ ~module_:__MODULE__ ) ;
              return ()))
      in
      let%bind () =
        with_label __LOC__ Field.Checked.Assert.(equal next_top_hash top_hash)
      in
      let%bind prev_state_valid =
        prev_state_valid wrap_vk_section wrap_vk prev_state_hash
      in
      let%bind inductive_case_passed =
        with_label __LOC__ Boolean.(prev_state_valid && success)
      in
      let%bind is_base_case = State.Checked.is_base_hash next_state_hash in
      let%bind () =
        as_prover
          As_prover.(
            Let_syntax.(
              let%map prev_valid = read Boolean.typ prev_state_valid
              and success = read Boolean.typ success
              and is_base_case = read Boolean.typ is_base_case in
              let result = (prev_valid && success) || is_base_case in
              Logger.trace logger
                "transition system debug state: (previous valid=$prev_valid \
                ∧ update success=$success) ∨ base case=$is_base_case = \
                $result"
                ~location:__LOC__ ~module_:__MODULE__
                ~metadata:
                  [ ("prev_valid", `Bool prev_valid)
                  ; ("success", `Bool success)
                  ; ("is_base_case", `Bool is_base_case)
                  ; ("result", `Bool result) ]))
      in
      with_label __LOC__
        (Boolean.Assert.any [is_base_case; inductive_case_passed])

    let main_with_null_logger = main (Logger.null ())

    let create_keys () = generate_keypair main_with_null_logger ~exposing:(step_input ())
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