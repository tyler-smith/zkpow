(* open Core_kernel *)
open Snark_params.Tick
open Zkpow_base

module Poly : sig
  type ( 'state_hash
       , 'height
       , 'weight )
       t =
    { previous_state_hash: 'state_hash
    ; height: 'height
    ; weight: 'weight }
  [@@deriving sexp, fields]
end

module Value : sig
  type t =
    ( State_hash.t
    , Field.t
    , Field.t )
    Poly.t
  [@@deriving sexp]
end

type value = Value.t

type var =
  ( State_hash.var
  , Field.Var.t
  , Field.Var.t)
  Poly.t

include
  Snark_params.Tick.Snarkable.S with type value := Value.t and type var := var

val create_value :
     previous_state_hash:State_hash.t
  -> height:Field.t
  -> weight:Field.t
  -> unit
  -> Value.t

(* val genesis : Value.t Lazy.t *)

val previous_state_hash :
  ('state_hash, _, _) Poly.t -> 'state_hash

val height :
  (_, 'height, _) Poly.t -> 'height

val weight :
  (_, _, 'weight) Poly.t -> 'weight