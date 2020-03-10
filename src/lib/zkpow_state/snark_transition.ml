open Core_kernel

module Field = Snark_params.Tick.Field

module Poly = struct
  type ( 'state_hash
       , 'height
       , 'weight )
       t =
    { previous_state_hash: 'state_hash
    ; height: 'height
    ; weight: 'weight }
  [@@deriving to_yojson, sexp, fields]
end

module Value = struct
  type t =
    ( State_hash.t
    , Field.t
    , Field.t )
    Poly.t
  [@@deriving sexp]
end

[%%define_locally
Poly.
  ( previous_state_hash
  , height
  , weight )]

type value = Value.t

type var =
  ( State_hash.var
  , Field.Var.t
  , Field.Var.t )
  Poly.t

let create_value ~previous_state_hash ~height ~weight () : Value.t =
  { previous_state_hash
  ; height
  ; weight }

(* let genesis : value lazy_t =
  lazy
    { Poly.height= Field.zero
    , weight= Field.zero } *)

let to_hlist
    { Poly.previous_state_hash
    ; height
    ; weight } =
  Snarky.H_list.
    [ previous_state_hash
    ; height
    ; weight ]

let of_hlist
    ([ previous_state_hash
     ; height
     ; weight ] :
      (unit, _) Snarky.H_list.t) =
  { Poly.previous_state_hash
  ; height
  ; weight }

let typ =
  let open Snark_params.Tick.Typ in
  of_hlistable ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
    ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist
    [ State_hash.typ
    ; Field.typ
    ; Field.typ ]