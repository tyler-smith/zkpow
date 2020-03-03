[%%import
"../../../config.mlh"]

open Ppxlib
open Asttypes
open Parsetree
open Longident
open Core

module Keys = struct
  module Proving = struct
    let key_location ~loc bc_location =
      let module E = Ppxlib.Ast_builder.Make (struct
        let loc = loc
      end) in
      let open E in
      estring
        (Blockchain_snark.Blockchain_transition.Keys.Proving.Location.to_string
           bc_location)

    let load_expr ~loc bc_location bc_checksum =
      let module E = Ppxlib.Ast_builder.Make (struct
        let loc = loc
      end) in
      let open E in
      [%expr
        let open Async.Deferred in
        Blockchain_snark.Blockchain_transition.Keys.Proving.load
          (Blockchain_snark.Blockchain_transition.Keys.Proving.Location
           .of_string
             [%e key_location ~loc bc_location])
        >>| fun (keys, checksum) ->
        assert (
          String.equal (Md5_lib.to_hex checksum)
            [%e estring (Md5_lib.to_hex bc_checksum)] ) ;
        keys]
  end

  module Verification = struct
    let key_location ~loc bc_location =
      let module E = Ppxlib.Ast_builder.Make (struct
        let loc = loc
      end) in
      let open E in
      estring
        (Blockchain_snark.Blockchain_transition.Keys.Verification.Location
         .to_string bc_location)

    let load_expr ~loc bc_location bc_checksum =
      let module E = Ppxlib.Ast_builder.Make (struct
        let loc = loc
      end) in
      let open E in
      [%expr
        let open Async.Deferred in
        Blockchain_snark.Blockchain_transition.Keys.Verification.load
          (Blockchain_snark.Blockchain_transition.Keys.Verification.Location
           .of_string
             [%e key_location ~loc bc_location])
        >>| fun (keys, checksum) ->
        assert (
          String.equal (Md5_lib.to_hex checksum)
            [%e estring (Md5_lib.to_hex bc_checksum)] ) ;
        keys]
  end
end


module Dummy = struct
  module Proving = struct
    let expr ~loc =
      [%expr
        Async.return
          Blockchain_snark.Blockchain_transition.Keys.Proving.dummy]
  end

  module Verification = struct
    let expr ~loc =
      [%expr
        Async.return
          Blockchain_snark.Blockchain_transition.Keys.Verification.dummy]
  end
end

open Async

let loc = Ppxlib.Location.none

[%%if
proof_level = "full"]

let gen_keys () =
  let open Async_kernel in
  let%bind {Cached.With_track_generated.data= acc; dirty} =
    let open Cached.Deferred_with_track_generated.Let_syntax in
    let module M = Zkpow.Transition.Make in
    let%map keys_location, _keys, keys_checksum = M.Keys.cached () in
    ( Keys.Proving.load_expr ~loc keys_location.proving
        keys_checksum.proving
    , Keys.Proving.key_location ~loc keys_location.proving
    , Keys.Verification.load_expr ~loc
        keys_location.verification keys_checksum.verification
    , Keys.Verification.key_location ~loc
        keys_location.verification )
  in
  match dirty with
  | `Generated_something -> (
    (* TODO: Check if circleci and die *)
    match (Sys.getenv "CI", Sys.getenv "DUNE_PROFILE") with
    | Some _, Some profile
      when String.is_substring ~substring:"testnet" profile ->
        exit 0xc1 (* exit with code 0xc1, get it "CI" *)
    | Some _, Some _ | _, None | None, _ ->
        return acc )
  | `Cache_hit ->
      return acc

[%%else]

let gen_keys () =
  let dummy_loc = [%expr "dummy-location"] in
  return
    ( Dummy.Proving.expr ~loc
    , dummy_loc
    , Dummy.Verification.expr ~loc
    , dummy_loc )

[%%endif]

let main () =
  let%bind ( bc_proving
           , bc_proving_loc
           , bc_verification
           , bc_verification_loc ) =
    gen_keys ()
  in
  let fmt =
    Format.formatter_of_out_channel (Out_channel.create "snark_keys.ml")
  in
  Pprintast.top_phrase fmt
    (Ptop_def
       [%str
         open Core_kernel

         let proving () = [%e bc_proving]

         let verification () = [%e bc_verification]

         type key_hashes = string list [@@deriving to_yojson]

         let key_locations =
           [ ("proving", [%e bc_proving_loc])
           ; ("verification", [%e bc_verification_loc]) ]

         let rec location_sexp_to_hashes = function
           | Sexp.Atom s
             when List.mem
                    ["base"; "merge"; "step"; "wrap"]
                    s ~equal:String.equal ->
               []
           | Sexp.Atom s -> (
               let fn = Filename.basename s in
               match String.split fn ~on:'_' with
               | hash :: _ ->
                   [hash]
               | _ ->
                   failwith "location_sexp_to_hashes: unexpected filename" )
           | Sexp.List sexps ->
               List.(concat (map sexps ~f:location_sexp_to_hashes))

         let location_to_hashes (loc : string) =
           match Sexp.parse loc with
           | Done (sexp, _) ->
               location_sexp_to_hashes sexp
           | _ ->
               []

         let key_hashes =
           let locations =
             List.map key_locations ~f:(fun (_name, loc) -> loc)
           in
           let hashes = List.(concat (map locations ~f:location_to_hashes)) in
           List.dedup_and_sort hashes ~compare:String.compare]) ;
  exit 0

let () =
  ignore (main ()) ;
  never_returns (Scheduler.go ())
