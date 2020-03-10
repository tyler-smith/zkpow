(* state_hash.ml -- defines the type for the protocol state hash *)

include Data_hash.Make_full_size ()

module Base58_check = Codable.Make_base58_check (struct
  include Stable.Latest

  let description = "State hash"

  let version_byte = Base58_check.Version_bytes.data_hash
end)

[%%define_locally
Base58_check.(to_base58_check, of_base58_check, of_base58_check_exn)]

[%%define_locally
Base58_check.String_ops.(to_string, of_string)]

(* let dummy = of_hash Outside_pedersen_image.t *)

let zero = Snark_params.Tick.Pedersen.zero_hash

let raw_hash_bytes = to_bytes

let to_bytes = `Use_to_base58_check_or_raw_hash_bytes

let to_decimal_string = to_decimal_string
