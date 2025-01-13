open! Core

type t' =
  | Unit
  | Never
  | Bool
  | Int
  | Nat
  | String
  | Chain_id
  | Bytes
  | Mutez
  | Key_hash
  | Key
  | Signature
  | Timestamp
  | Address
  | Option of t
  | List of t
  | Set of t
  | Operation
  | Contract of t
  | Ticket of t
  | Pair of t * t
  | Or of t * t
  | Lambda of t * t
  | Map of t * t
  | Big_map of t * t
  | Bls12_381_g1
  | Bls12_381_g2
  | Bls12_381_fr
  | Sapling_transaction of Bigint.t
  | Sapling_state of Bigint.t
  | Chest
  | Chest_key

and t = t' * Common_adt.Annot.t list

include Sexpable.S with type t := t
include Comparable.S with type t := t

val is_comparable_type : t -> bool
val is_packable : t -> bool
val is_contract_type_compatible : t -> t -> bool
val t'_to_string : t' -> string
val to_string : t -> string
val has_annot : Common_adt.Annot.t -> t -> bool
val are_compatible : t -> t -> bool
