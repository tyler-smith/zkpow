(* open Coda_base *)
open Core_kernel
open Snark_params
open Snark_bits
open Tick
open Bitstring_lib

let cache_dir = "/Users/tyler/dev/zkpow/test-data"

let cache_dir_possible_paths = [cache_dir]

module Digest = Tick.Pedersen.Digest
module Storage = Storage.List.Make (Storage.Disk)

module Keys0 = struct
  module Per_curve_location = struct
    module T = struct
      type t = {step: Storage.location; wrap: Storage.location}
      [@@deriving sexp]
    end

    include T
    include Sexpable.To_stringable (T)
  end

  module Proving = struct
    module Location = Per_curve_location

    let checksum ~step ~wrap =
      Md5.digest_string(Md5.to_hex step ^ Md5.to_hex wrap)

    type t = {step: Tick.Proving_key.t; wrap: Tock.Proving_key.t}

    let dummy =
      { step= Dummy_values.Tick.Groth16.proving_key
      ; wrap= Dummy_values.Tock.Bowe_gabizon18.proving_key }

    let load ({step; wrap} : Location.t) =
      let open Storage in
      let logger = Logger.create () in
      let tick_controller =
        Controller.create ~logger (module Tick.Proving_key)
      in
      let tock_controller =
        Controller.create ~logger (module Tock.Proving_key)
      in
      let open Async in
      let load c p =
        match%map load_with_checksum c p with
        | Ok x ->
            x
        | Error e ->
            failwithf
              !"Blockchain_snark: load failed on %{sexp:Storage.location}: \
                %{sexp:[`Checksum_no_match|`No_exist|`IO_error of Error.t]}"
              p e ()
      in
      let%map step = load tick_controller step
      and wrap = load tock_controller wrap in
      let t = {step= step.data; wrap= wrap.data} in
      (t, checksum ~step:step.checksum ~wrap:wrap.checksum)
  end

  module Verification = struct
    module Location = Per_curve_location

    let checksum ~step ~wrap =
      Md5.digest_string
        ( "Blockchain_transition_verification" ^ Md5.to_hex step
        ^ Md5.to_hex wrap )

    type t = {step: Tick.Verification_key.t; wrap: Tock.Verification_key.t}

    let dummy =
      { step=
          Tick_backend.Verification_key.get_dummy
            ~input_size:Transition_system.step_input_size
      ; wrap=
          Tock_backend.Bowe_gabizon.Verification_key.get_dummy
            ~input_size:Wrap_input.size }

    let load ({step; wrap} : Location.t) =
      let open Storage in
      let logger = Logger.create () in
      let tick_controller =
        Controller.create ~logger (module Tick.Verification_key)
      in
      let tock_controller =
        Controller.create ~logger (module Tock.Verification_key)
      in
      let open Async in
      let load c p =
        match%map load_with_checksum c p with
        | Ok x ->
            x
        | Error _e ->
            failwithf
              !"Blockchain_snark: load failed on %{sexp:Storage.location}"
              p ()
      in
      let%map step = load tick_controller step
      and wrap = load tock_controller wrap in
      let t = {step= step.data; wrap= wrap.data} in
      (t, checksum ~step:step.checksum ~wrap:wrap.checksum)
  end

  type t = {proving: Proving.t; verification: Verification.t}

  let dummy = {proving= Proving.dummy; verification= Verification.dummy}

  module Checksum = struct
    type t = {proving: Md5.t; verification: Md5.t}
  end

  module Location = struct
    module T = struct
      type t =
        {proving: Proving.Location.t; verification: Verification.Location.t}
      [@@deriving sexp]
    end

    include T
    include Sexpable.To_stringable (T)
  end

  (* let load ({proving; verification} : Location.t) =
    let%map proving, proving_checksum = Proving.load proving
    and verification, verification_checksum = Verification.load verification in
    ( {proving; verification}
    , {Checksum.proving= proving_checksum; verification= verification_checksum}
    ) *)
end

module Base = struct
  open Fold_lib

  module Snark = Transition_system.Make (struct
    module Tick = struct
      module Packed = struct
        type value = Tick.Pedersen.Digest.t

        type var = Tick.Pedersen.Checked.Digest.var

        let typ = Tick.Pedersen.Checked.Digest.typ
      end

      module Unpacked = struct
        type value = Tick.Pedersen.Checked.Digest.Unpacked.t

        type var = Tick.Pedersen.Checked.Digest.Unpacked.var

        let typ : (var, value) Tick.Typ.t =
          Tick.Pedersen.Checked.Digest.Unpacked.typ

        let var_to_bits (x : var) =
          Bitstring_lib.Bitstring.Lsb_first.of_list
            (x :> Tick.Boolean.var list)

        let var_of_bits = Fn.id

        let var_to_triples xs =
          let open Fold in
          to_list
            (group3 ~default:Tick.Boolean.false_
                (of_list (var_to_bits xs :> Tick.Boolean.var list)))

        let var_of_value =
          Tick.Pedersen.Checked.Digest.Unpacked.constant
      end

      let project_value =
        Fn.compose Tick.Field.project Bitstring.Lsb_first.to_list

      let project_var = Tick.Pedersen.Checked.Digest.Unpacked.project

      let unpack_value =
        Fn.compose Bitstring.Lsb_first.of_list Tick.Field.unpack

      let choose_preimage_var =
        Tick.Pedersen.Checked.Digest.choose_preimage
    end

    module Tock = Bits.Snarkable.Field (Tock)
  end)

  module Keys = struct
    open Snark
    include Keys0
  
    let step_cached =
      let load =
        let open Tick in
        let open Cached.Let_syntax in
        let%map verification =
          Cached.component ~label:"step_verification" ~f:Keypair.vk
            (module Verification_key)
        and proving =
          Cached.component ~label:"step_proving" ~f:Keypair.pk
            (module Proving_key)
        in
        (verification, {proving with value= ()})
      in
      Cached.Spec.create ~load ~name:"blockchain-snark step keys"
        ~autogen_path:""
        ~manual_install_path:cache_dir
        ~brew_install_path:""
        ~s3_install_path:""
        ~digest_input:
          (Fn.compose Md5.to_hex Tick.R1CS_constraint_system.digest)
        ~create_env:Tick.Keypair.generate
        ~input:
          (Tick.constraint_system ~exposing:(Step_base.input ()) Step_base.main)
  
    let cached () =
      let open Cached.Deferred_with_track_generated.Let_syntax in
      let paths = Fn.compose Cache_dir.possible_paths Filename.basename in
      let%bind step_vk, step_pk = Cached.run step_cached in
      let module Wrap = Wrap_base (struct
        let verification_key = step_vk.value
      end) in
      let wrap_cached =
        let load =
          let open Tock in
          let open Cached.Let_syntax in
          let%map verification =
            Cached.component ~label:"wrap_verification" ~f:Keypair.vk
              (module Verification_key)
          and proving =
            Cached.component ~label:"wrap_proving" ~f:Keypair.pk
              (module Proving_key)
          in
          (verification, {proving with value= ()})
        in
        Cached.Spec.create ~load ~name:"blockchain-snark wrap keys"
          ~autogen_path:""
          ~manual_install_path:cache_dir
          ~brew_install_path:""
          ~s3_install_path:""
          ~digest_input:(fun x ->
            Md5.to_hex (Tock.R1CS_constraint_system.digest (Lazy.force x)) )
          ~input:(lazy (Tock.constraint_system ~exposing:Wrap.input Wrap.main))
          ~create_env:(fun x -> Tock.Keypair.generate (Lazy.force x))
      in
      let%map wrap_vk, wrap_pk = Cached.run wrap_cached in
      let location : Location.t =
        { proving= {step= paths step_pk.path; wrap= paths wrap_pk.path}
        ; verification= {step= paths step_vk.path; wrap= paths wrap_vk.path} }
      in
      let checksum : Checksum.t =
        { proving=
            Proving.checksum ~step:step_pk.checksum ~wrap:wrap_pk.checksum
        ; verification=
            Verification.checksum ~step:step_vk.checksum ~wrap:wrap_vk.checksum
        }
      in
      let t : Verification.t = {step= step_vk.value; wrap= wrap_vk.value} in
      (location, t, checksum)
  end

  module State = Snark.Step_base.Prover_state


  let step_kp = Snark.Step_base.create_keys ()
  let step_pk = Tick.Keypair.pk step_kp
  let step_vk = Tick.Keypair.vk step_kp

  module Step = Snark.Step(struct
    let keys = step_kp
  end)

  module Step_vk = struct
    let verification_key = Tick.Keypair.vk step_kp
  end

  module Wrap_base = Snark.Wrap_base(Step_vk)

  let wrap_kp = Wrap_base.create_keys ()
  let wrap_pk = Tock.Keypair.pk wrap_kp
  let wrap_vk = Tock.Keypair.vk wrap_kp

  module Wrap = Snark.Wrap (Step_vk)(struct
    let keys = wrap_kp
  end)

  module WrappedState = Wrap_base.Prover_state
end

