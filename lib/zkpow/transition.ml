(* open Coda_base *)


  (* include Transition_system.Make (struct
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
    include Keys

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
        ~autogen_path:Cache_dir.autogen_path
        ~manual_install_path:Cache_dir.manual_install_path
        ~brew_install_path:Cache_dir.brew_install_path
        ~s3_install_path:Cache_dir.s3_install_path
        ~digest_input:
          (Fn.compose Md5.to_hex Tick.R1CS_constraint_system.digest)
        ~create_env:Tick.Keypair.generate
        ~input:
          (Tick.constraint_system ~exposing:(Step_base.input ())
             (Step_base.main (Logger.null ())))

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
          ~autogen_path:Cache_dir.autogen_path
          ~manual_install_path:Cache_dir.manual_install_path
          ~brew_install_path:Cache_dir.brew_install_path
          ~s3_install_path:Cache_dir.s3_install_path
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
  end *)