open! Core

type annot = Common_adt.Annot.t [@@deriving ord, sexp]
type 'a node = 'a Adt.node [@@deriving ord, sexp]

type data_t =
  | D_int of Bigint.t
  | D_nat of Bigint.t
  | D_string of string
  | D_bytes of Bytes.t
  | D_unit
  | D_bool of bool
  | D_pair of data * data
  | D_left of data
  | D_right of data
  | D_some of data
  | D_none
  | D_list of data list
  | D_map of (data * data) list
  | D_instruction of inst

and data = (Adt.typ * data_t) node

and inst_t =
  | I_abs
  | I_add_nat
  | I_add_nat_int
  | I_add_int
  | I_add_timestamp_int
  | I_add_mutez
  | I_add_bls12_381_g1
  | I_add_bls12_381_g2
  | I_add_bls12_381_fr
  | I_address
  | I_amount
  | I_and_bool
  | I_and_nat
  | I_and_int_nat
  | I_apply
  | I_balance
  | I_blake2b
  | I_car
  | I_cdr
  | I_chain_id
  | I_check_signature
  | I_compare
  | I_concat_string
  | I_concat_list_string
  | I_concat_bytes
  | I_concat_list_bytes
  | I_cons
  | I_contract of Adt.typ
  | I_create_contract of program
  | I_dig of Bigint.t
  | I_dip of inst
  | I_dip_n of Bigint.t * inst
  | I_drop of Bigint.t
  | I_dug of Bigint.t
  | I_dup
  | I_dup_n of Bigint.t
  | I_ediv_nat
  | I_ediv_nat_int
  | I_ediv_int
  | I_ediv_mutez_nat
  | I_ediv_mutez
  | I_empty_big_map of Adt.typ * Adt.typ
  | I_empty_map of Adt.typ * Adt.typ
  | I_empty_set of Adt.typ
  | I_eq
  | I_exec
  | I_failwith
  | I_ge
  | I_get_map
  | I_get_big_map
  | I_get_n of Bigint.t
  | I_get_and_update_map
  | I_get_and_update_big_map
  | I_gt
  | I_hash_key
  | I_if of inst * inst
  | I_if_cons of inst * inst
  | I_if_left of inst * inst
  | I_if_none of inst * inst
  | I_implicit_account
  | I_int_nat
  | I_int_bls12_381_fr
  | I_isnat
  | I_iter_set of inst
  | I_iter_map of inst
  | I_iter_list of inst
  | I_join_tickets
  | I_keccak
  | I_lambda of Adt.typ * Adt.typ * inst
  | I_le
  | I_left of Adt.typ
  | I_level
  | I_loop of inst
  | I_loop_left of inst
  | I_lsl
  | I_lsr
  | I_lt
  | I_map_list of inst
  | I_map_map of inst
  | I_mem_set
  | I_mem_map
  | I_mem_big_map
  | I_mul_nat
  | I_mul_nat_int
  | I_mul_int
  | I_mul_mutez_nat
  | I_mul_bls12_381_g1_bls12_381_fr
  | I_mul_bls12_381_g2_bls12_381_fr
  | I_mul_bls12_381_fr_bls12_381_fr
  | I_mul_nat_bls12_381_fr
  | I_mul_int_bls12_381_fr
  | I_neg_nat
  | I_neg_int
  | I_neg_bls12_381_g1
  | I_neg_bls12_381_g2
  | I_neg_bls12_381_fr
  | I_neq
  | I_never
  | I_nil of Adt.typ
  | I_none of Adt.typ
  | I_not_bool
  | I_not_nat
  | I_not_int
  | I_now
  | I_or_bool
  | I_or_nat
  | I_pack
  | I_pair
  | I_pair_n of Bigint.t
  | I_pairing_check
  | I_push of data
  | I_read_ticket
  | I_right of Adt.typ
  | I_sapling_empty_state of Bigint.t
  | I_sapling_verify_update
  | I_self
  | I_self_address
  | I_sender
  | I_set_delegate
  | I_sha256
  | I_sha512
  | I_sha3
  | I_size_set
  | I_size_map
  | I_size_list
  | I_size_string
  | I_size_bytes
  | I_slice_string
  | I_slice_bytes
  | I_some
  | I_source
  | I_split_ticket
  | I_sub_nat
  | I_sub_nat_int
  | I_sub_int
  | I_sub_timestamp_int
  | I_sub_timestamp
  | I_sub_mutez
  | I_swap
  | I_ticket
  | I_total_voting_power
  | I_transfer_tokens
  | I_unit
  | I_unpack of Adt.typ
  | I_unpair of Bigint.t
  | I_update_set
  | I_update_map
  | I_update_big_map
  | I_update_n of Bigint.t
  | I_update_n_pair of Bigint.t
  | I_voting_power
  | I_xor_bool
  | I_xor_nat
  | I_seq of inst list
  | I_noop
  | I_open_chest
  | I_cast of Adt.typ
  | I_create_account
[@@deriving sexp, ord]

and inst = (inst_t * annot list) node
and seq = Seq_i of inst | Seq_s of inst * seq
and program = { param : Adt.typ; storage : Adt.typ; code : inst }
