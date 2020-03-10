open Snark_params.Tick
open Zkpow_base

module Update : Snarkable.S

module Poly : sig
  type ('psh, 'field) t =
    { previous_state_hash: 'psh
    ; height: 'field
    ; weight: 'field }
    [@@deriving eq, sexp, hash, yojson, compare, fields]
end

module Value : sig
  type t =
    (State_hash.t, Field.t) Poly.t
  [@@deriving sexp, compare, eq]
  (* [@@deriving sexp, bin_io, compare, eq, to_yojson] *)

  (* include Hashable.S with type t := t *)
end

module Hash : sig
  type t [@@deriving sexp]

  type var

  val typ : (var, t) Typ.t

  val var_to_field : var -> Field.Var.t

  val of_hash : Pedersen.Digest.t -> t
  
  val var_of_t : t -> var
end

type var = (Hash.var, Field.Var.t) Poly.t

type value = Value.t [@@deriving sexp]

val typ : (var, value) Typ.t

val hash : value -> Hash.t

module Checked : sig
  val hash : var -> (Hash.var, _) Checked.t

  val is_base_hash : Hash.var -> (Boolean.var, _) Checked.t

  val update :
      logger:Logger.t
    -> Hash.var * var
    -> Update.var
    -> (Hash.var * var * [`Success of Boolean.var], _) Checked.t
end

val create : previous_state_hash:'a -> height:'b -> weight:'b -> ('a, 'b) Poly.t

val create_value :
     previous_state_hash:Hash.t
  -> height:Field.t
  -> weight:Field.t
  -> value

val create_var :
     previous_state_hash:Hash.var
  -> height:Field.Var.t
  -> weight:Field.Var.t
  -> var

val previous_state_hash : ('a, _) Poly.t -> 'a

val height : (_, 'a) Poly.t -> 'a

val weight : (_, 'a) Poly.t -> 'a

val hash_checked : var -> (State_hash.var, _) Checked.t

val hash : Value.t -> State_hash.t