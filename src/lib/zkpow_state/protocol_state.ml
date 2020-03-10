open Core_kernel
open Snark_params.Tick
open Zkpow_base

module Update = Snark_transition

module Poly = struct
  type ( 'state_hash
       , 'field )
       t =
       { previous_state_hash: 'state_hash
       ; height: 'field
       ; weight: 'field }
  [@@deriving eq, sexp, hash, yojson, compare, fields]
end

let previous_state_hash {Poly.previous_state_hash; _} =
  previous_state_hash

let height {Poly.height; _} = height

let weight {Poly.weight; _} = weight

module Value = struct
  type t =
    (State_hash.t, Field.t) Poly.t
  [@@deriving sexp, hash, compare, equal]
    (* [@@deriving sexp, hash, compare, eq, to_yojson] *)

  (* include Hashable.Make (Stable.Latest) *)
end

module Hash = struct
  include State_hash
  let var_to_field = var_to_hash_packed
end

type var = (Hash.var, Field.Var.t) Poly.t

type value = Value.t [@@deriving sexp]
(* type value = (Hash.t, Field.t) Poly.t [@@deriving sexp] *)

let create ~previous_state_hash ~height ~weight =
  {Poly.previous_state_hash; height; weight}

let create' ~previous_state_hash ~height ~weight =
  {Poly.previous_state_hash; height; weight}

let create_value = create'

let create_var = create'

let hash ({previous_state_hash; height; weight} : value) = 
  (* Curve_choice.Tick0.Field.zero *)
  Random_oracle.hash [|previous_state_hash;height;weight|]

let hash_checked ({previous_state_hash; height; weight} : var) =
  let open Random_oracle.Input in
  let input = Hash.var_to_input previous_state_hash in
  let input = append input (field height) in
  let input = append input (field weight) in
  make_checked (fun () ->
    Hash.var_of_hash_packed Random_oracle.Checked.(
      hash (pack_input input)
    )
  )

let typ =
  let module H_list = Snarky.H_list in
  let open Snarky.H_list in
  let spec = Data_spec.[Hash.typ; Field.typ; Field.typ] in
  let to_hlist {Poly.previous_state_hash; height; weight} = [previous_state_hash; height; weight] in
  let of_hlist : 'psh 'f. (unit, 'psh -> 'f -> 'f -> unit) H_list.t -> ('psh, 'f) Poly.t =
    fun H_list.[previous_state_hash; height; weight] -> {previous_state_hash; height; weight} in
  (* let of_hlist : (unit, 'psh -> 'field -> unit) H_list.t -> ('psh, 'field) Poly.t =
    fun [previous_state_hash; height; weight] -> {Poly.previous_state_hash; height; weight}
  in *)
  Typ.of_hlistable spec ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
    ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist

module Checked = struct
  let genesis_hash = 
    Field.zero

  let%snarkydef is_base_hash h =
    Field.Checked.equal
      (Field.Var.constant
          (genesis_hash :> Field.t))
      (State_hash.var_to_hash_packed h)
  let%snarkydef hash (t : var) = hash_checked t

  let%snarkydef update ~(logger : Logger.t)
  ((previous_state_hash, previous_state) :
    State_hash.var * var)
    (_ : Snark_transition.var) :
    (* (transition : Snark_transition.var) : *)
  ( State_hash.var * var * [`Success of Boolean.var]
  , _ )
  Checked.t =
    let () =
      Logger.trace logger "in Protocol_state.Checked.update" ~module_:__MODULE__ ~location:__LOC__
    in
    (* Check nbits *)
    (* Check timestamp *)
    let h = height previous_state in
    let next_height = Field.Var.add h (Field.Var.constant Field.one) in
    let new_state =
      create_var ~previous_state_hash ~height:next_height ~weight:next_height
    in
    let%map state_hash = hash_checked new_state in
    (state_hash, new_state, `Success Boolean.true_)
end