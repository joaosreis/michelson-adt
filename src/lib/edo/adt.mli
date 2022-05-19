open! Core
open Common_adt

type annot = Common_adt.Annot.t [@@deriving ord, sexp]
type 'a node = 'a Common_adt.Node.t [@@deriving ord, sexp]

type typ_t =
  | T_unit
  | T_never
  | T_bool
  | T_int
  | T_nat
  | T_string
  | T_chain_id
  | T_bytes
  | T_mutez
  | T_key_hash
  | T_key
  | T_signature
  | T_timestamp
  | T_address
  | T_option of typ
  | T_list of typ
  | T_set of typ
  | T_operation
  | T_contract of typ
  | T_ticket of typ
  | T_pair of typ * typ
  | T_or of typ * typ
  | T_lambda of typ * typ
  | T_map of typ * typ
  | T_big_map of typ * typ
  | T_bls12_381_g1
  | T_bls12_381_g2
  | T_bls12_381_fr
  | T_sapling_transaction of Bigint.t
  | T_sapling_state of Bigint.t
  | T_chest
  | T_chest_key

and typ = (typ_t * annot list) node [@@deriving ord, sexp]

type inst_t =
  | I_noop
  | I_failwith
  | I_seq of inst list
  | I_if of inst * inst
  | I_loop of inst
  | I_loop_left of inst
  | I_dip of inst
  | I_dip_n of Bigint.t * inst
  | I_exec
  | I_apply
  | I_drop
  | I_drop_n of Bigint.t
  | I_dup of Bigint.t
  | I_swap
  | I_dig of Bigint.t
  | I_dug of Bigint.t
  | I_push of typ * data
  | I_unit
  | I_lambda of typ * typ * inst
  | I_eq
  | I_neq
  | I_lt
  | I_gt
  | I_le
  | I_ge
  | I_or
  | I_and
  | I_xor
  | I_not
  | I_neg
  | I_abs
  | I_isnat
  | I_int
  | I_add
  | I_sub
  | I_mul
  | I_ediv
  | I_lsl
  | I_lsr
  | I_compare
  | I_concat
  | I_size
  | I_slice
  | I_pair
  | I_car
  | I_cdr
  | I_empty_set of typ
  | I_mem
  | I_update
  | I_iter of inst
  | I_empty_map of typ * typ
  | I_get
  | I_map of inst
  | I_empty_big_map of typ * typ
  | I_some
  | I_none of typ
  | I_if_none of inst * inst
  | I_left of typ
  | I_right of typ
  | I_if_left of inst * inst
  | I_cons
  | I_nil of typ
  | I_if_cons of inst * inst
  | I_create_contract of program
  | I_create_account
  | I_transfer_tokens
  | I_set_delegate
  | I_balance
  | I_address
  | I_contract of typ
  | I_source
  | I_sender
  | I_self
  | I_amount
  | I_implicit_account
  | I_voting_power
  | I_now
  | I_chain_id
  | I_pack
  | I_unpack of typ
  | I_hash_key
  | I_blake2b
  | I_keccak
  | I_sha3
  | I_sha256
  | I_sha512
  | I_check_signature
  | I_cast of typ
  | I_unpair
  | I_unpair_n of Bigint.t
  | I_rename
  | I_total_voting_power
  | I_pairing_check
  | I_sapling_empty_state of Bigint.t
  | I_sapling_verify_update
  | I_ticket
  | I_read_ticket
  | I_split_ticket
  | I_join_tickets
  | I_never
  | I_self_address
  | I_level
  | I_pair_n of Bigint.t
  | I_get_n of Bigint.t
  | I_update_n of Bigint.t
  | I_open_chest
  | I_get_and_update

and inst = (inst_t * annot list) node [@@deriving ord, sexp]

and data_t =
  | D_int of Bigint.t
  | D_string of string
  | D_bytes of Bytes.t
  | D_unit
  | D_bool of bool
  | D_pair of data * data
  | D_left of data
  | D_right of data
  | D_some of data
  | D_none
  | D_elt of data * data
  | D_list of data list
  | D_instruction of inst

and data = data_t node [@@deriving ord, sexp]
and program = { param : typ; storage : typ; code : inst } [@@deriving ord, sexp]

module Typ : sig
  type t = typ

  include Comparable.S with type t := t
  include Sexpable.S with type t := t

  val create : int -> ?location:Loc.t -> ?annots:annot list -> typ_t -> t
  val has_annot : Common_adt.Annot.t -> t -> bool
  val to_string : t -> string
end

module Data : sig
  type t = data

  include Comparable.S with type t := t
  include Sexpable.S with type t := t

  val create : int -> ?location:Loc.t -> data_t -> t
end

module Inst : sig
  type t = inst

  include Comparable.S with type t := t
  include Sexpable.S with type t := t

  val create : int -> ?location:Loc.t -> ?annots:Annot.t list -> inst_t -> t
end
