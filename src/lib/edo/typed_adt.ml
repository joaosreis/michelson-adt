open! Containers
open Common_adt

type annot = Common_adt.Annot.t [@@deriving ord, eq]
type 'a node = 'a Adt.node [@@deriving ord, eq]
type typ_t = Adt.typ_t [@@deriving ord, eq]
type typ = Adt.typ [@@deriving ord, eq]

type data_t =
  | D_int of Z.t
  | D_nat of Z.t
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

and data = (typ * data_t) node

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
  | I_dig of Z.t
  | I_dip of inst
  | I_dip_n of Z.t * inst
  | I_drop of Z.t
  | I_dug of Z.t
  | I_dup of Z.t
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
  | I_get_n of Z.t
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
  | I_pair_n of Z.t
  | I_pairing_check
  | I_push of data
  | I_read_ticket
  | I_right of Adt.typ
  | I_sapling_empty_state of Z.t
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
  | I_unpair of Z.t
  | I_update_set
  | I_update_map
  | I_update_big_map
  | I_update_n of Z.t
  | I_voting_power
  | I_xor_bool
  | I_xor_nat
  | I_seq of inst list
  | I_noop
  | I_open_chest
  | I_cast of Adt.typ
  | I_create_account
[@@deriving ord]

and inst = (inst_t * annot list) node
and seq = Seq_i of inst | Seq_s of inst * seq
and program = { param : typ; storage : typ; code : inst } [@@deriving eq]

module Typ = Adt.Typ

module rec Data : sig
  type t = data [@@deriving ord]

  val create : int -> ?location:Loc.t -> Typ.t -> data_t -> t
  val pp : Format.formatter -> t -> unit
end = struct
  type t = data [@@deriving ord]

  let create id ?(location = Loc.dummy_loc) t d = Node.create id ~location (t, d)

  let rec pp ppf (d : t) =
    let open Format in
    match snd d.Common_adt.Node.value with
    | D_int d | D_nat d -> Z.pp_print ppf d
    | D_string s -> fprintf ppf "\"%s\"" s
    | D_bytes b -> fprintf ppf "%s" (Bytes.to_string b)
    | D_left d -> fprintf ppf "Left %a" pp d
    | D_right d -> fprintf ppf "Right %a" pp d
    | D_some d -> fprintf ppf "Some %a" pp d
    | D_none -> fprintf ppf "None"
    | D_unit -> fprintf ppf "Unit"
    | D_bool b -> fprintf ppf (match b with true -> "True" | false -> "False")
    | D_pair (d_1, d_2) -> fprintf ppf "(Pair %a %a)" pp d_1 pp d_2
    | D_list d -> pp_print_list pp ppf d
    | D_map d ->
        pp_print_list
          (fun ppf' (k, v) -> fprintf ppf' "Elt %a %a" pp k pp v)
          ppf d
    | D_instruction i -> Inst.pp ppf i
end

and Inst : sig
  type t = inst [@@deriving ord, eq]

  val create : int -> ?location:Loc.t -> ?annots:annot list -> inst_t -> t
  val pp : Format.formatter -> t -> unit
  val pp_inst_t : Format.formatter -> inst_t -> unit
end = struct
  type t = inst [@@deriving ord, eq]

  let create id ?(location = Loc.dummy_loc) ?(annots = []) t =
    Node.create id ~location (t, annots)

  let rec pp_inst_t ppf =
    let open Format in
    function
    | I_abs -> fprintf ppf "ABS"
    | I_drop n -> fprintf ppf "DROP %a" Z.pp_print n
    | I_swap -> fprintf ppf "SWAP"
    | I_some -> fprintf ppf "SOME"
    | I_unit -> fprintf ppf "UNIT"
    | I_pair -> fprintf ppf "PAIR"
    | I_car -> fprintf ppf "CAR"
    | I_cdr -> fprintf ppf "CDR"
    | I_cons -> fprintf ppf "CONS"
    | I_size_bytes | I_size_list | I_size_string | I_size_set | I_size_map ->
        fprintf ppf "SIZE"
    | I_mem_set | I_mem_big_map | I_mem_map -> fprintf ppf "MEM"
    | I_get_map | I_get_big_map -> fprintf ppf "GET"
    | I_get_n n -> fprintf ppf "GET %a" Z.pp_print n
    | I_update_set | I_update_map | I_update_big_map -> fprintf ppf "UPDATE"
    | I_update_n n -> fprintf ppf "UPDATE %a" Z.pp_print n
    | I_exec -> fprintf ppf "EXEC"
    | I_failwith -> fprintf ppf "FAILWITH"
    | I_cast t -> fprintf ppf "CAST %a" Typ.pp t
    | I_concat_bytes | I_concat_list_bytes | I_concat_string
    | I_concat_list_string ->
        fprintf ppf "CONCAT"
    | I_slice_bytes | I_slice_string -> fprintf ppf "SLICE"
    | I_pack -> fprintf ppf "PACK"
    | I_add_bls12_381_fr | I_add_bls12_381_g1 | I_add_bls12_381_g2 | I_add_nat
    | I_add_nat_int | I_add_int | I_add_timestamp_int | I_add_mutez ->
        fprintf ppf "ADD"
    | I_sub_int | I_sub_nat | I_sub_nat_int | I_sub_timestamp_int
    | I_sub_timestamp | I_sub_mutez ->
        fprintf ppf "SUB"
    | I_mul_int | I_mul_nat | I_mul_nat_int | I_mul_mutez_nat
    | I_mul_bls12_381_g1_bls12_381_fr | I_mul_bls12_381_g2_bls12_381_fr
    | I_mul_bls12_381_fr_bls12_381_fr | I_mul_nat_bls12_381_fr
    | I_mul_int_bls12_381_fr ->
        fprintf ppf "MUL"
    | I_ediv_int | I_ediv_nat | I_ediv_nat_int | I_ediv_mutez_nat | I_ediv_mutez
      ->
        fprintf ppf "EDIV"
    | I_isnat -> fprintf ppf "ISNAT"
    | I_int_nat | I_int_bls12_381_fr -> fprintf ppf "INT"
    | I_neg_int | I_neg_nat | I_neg_bls12_381_g1 | I_neg_bls12_381_g2
    | I_neg_bls12_381_fr ->
        fprintf ppf "NEG"
    | I_lsl -> fprintf ppf "LSL"
    | I_lsr -> fprintf ppf "LSR"
    | I_or_bool | I_or_nat -> fprintf ppf "OR"
    | I_and_bool | I_and_nat | I_and_int_nat -> fprintf ppf "AND"
    | I_xor_bool | I_xor_nat -> fprintf ppf "XOR"
    | I_not_bool | I_not_nat | I_not_int -> fprintf ppf "NOT"
    | I_compare -> fprintf ppf "COMPARE"
    | I_eq -> fprintf ppf "EQ"
    | I_neq -> fprintf ppf "NEQ"
    | I_lt -> fprintf ppf "LT"
    | I_gt -> fprintf ppf "GT"
    | I_le -> fprintf ppf "LE"
    | I_ge -> fprintf ppf "GE"
    | I_self -> fprintf ppf "SELF"
    | I_transfer_tokens -> fprintf ppf "TRANSFER_TOKENS"
    | I_set_delegate -> fprintf ppf "SET_DELEGATE"
    | I_implicit_account -> fprintf ppf "IMPLICIT_ACCOUNT"
    | I_now -> fprintf ppf "NOW"
    | I_amount -> fprintf ppf "AMOUNT"
    | I_balance -> fprintf ppf "BALANCE"
    | I_check_signature -> fprintf ppf "CHECK_SIGNATURE"
    | I_blake2b -> fprintf ppf "BLAKE2B"
    | I_sha256 -> fprintf ppf "SHA256"
    | I_sha512 -> fprintf ppf "SHA512"
    | I_hash_key -> fprintf ppf "HASH_KEY"
    | I_source -> fprintf ppf "SOURCE"
    | I_sender -> fprintf ppf "SENDER"
    | I_address -> fprintf ppf "ADDRESS"
    | I_chain_id -> fprintf ppf "CHAIN_ID"
    | I_noop -> fprintf ppf ""
    | I_unpair n -> fprintf ppf "UNPAIR %a" Z.pp_print n
    | I_seq _ -> fprintf ppf "" (* TODO: *)
    | I_dig n -> fprintf ppf "DIG %a" Z.pp_print n
    | I_dug n -> fprintf ppf "DUG %a" Z.pp_print n
    | I_push d -> fprintf ppf "PUSH %a" Data.pp d
    | I_none t -> fprintf ppf "NONE %a" Typ.pp t
    | I_if_none (i_1, i_2) -> fprintf ppf "IF_NONE { %a } { %a }" pp i_1 pp i_2
    | I_left t -> fprintf ppf "LEFT %a" Typ.pp t
    | I_right t -> fprintf ppf "RIGHT %a" Typ.pp t
    | I_if_left (i_1, i_2) -> fprintf ppf "IF_LEFT { %a } { %a }" pp i_1 pp i_2
    | I_nil t -> fprintf ppf "NIL %a" Typ.pp t
    | I_if_cons (i_1, i_2) -> fprintf ppf "IF_CONS { %a } { %a }" pp i_1 pp i_2
    | I_empty_set t -> fprintf ppf "EMPTY_SET %a" Typ.pp t
    | I_empty_map (t_1, t_2) ->
        fprintf ppf "EMPTY_MAP %a %a" Typ.pp t_1 Typ.pp t_2
    | I_empty_big_map (t_1, t_2) ->
        fprintf ppf "EMPTY_BIG_MAP %a %a" Typ.pp t_1 Typ.pp t_2
    | I_map_list i | I_map_map i -> fprintf ppf "MAP { %a }" pp i
    | I_iter_list i | I_iter_map i | I_iter_set i ->
        fprintf ppf "ITER { %a }" pp i
    | I_if (i_1, i_2) -> fprintf ppf "IF { %a } { %a }" pp i_1 pp i_2
    | I_loop i -> fprintf ppf "LOOP { %a }" pp i
    | I_loop_left i -> fprintf ppf "LOOP_LEFT { %a }" pp i
    | I_lambda (t_1, t_2, i) ->
        fprintf ppf "LAMBDA %a %a { %a }" Typ.pp t_1 Typ.pp t_2 pp i
    | I_dip i -> fprintf ppf "DIP { %a }" pp i
    | I_dip_n (n, i) -> fprintf ppf "DIP %a { %a }" Z.pp_print n pp i
    | I_unpack t -> fprintf ppf "UNPACK %a" Typ.pp t
    | I_contract t -> fprintf ppf "CONTRACT %a" Typ.pp t
    | I_create_contract p -> fprintf ppf "CREATE_CONTRACT { %a }" pp_program p
    | I_create_account -> fprintf ppf "CREATE_ACCOUNT"
    | I_apply -> fprintf ppf "APPLY"
    | I_never -> fprintf ppf "NEVER"
    | I_self_address -> fprintf ppf "SELF_ADDRESS"
    | I_voting_power -> fprintf ppf "VOTING_POWER"
    | I_level -> fprintf ppf "LEVEL"
    | I_keccak -> fprintf ppf "KECCAK"
    | I_sha3 -> fprintf ppf "SHA3"
    | I_total_voting_power -> fprintf ppf "TOTAL_VOTING_POWER"
    | I_pairing_check -> fprintf ppf "PAIRING_CHECK"
    | I_sapling_verify_update -> fprintf ppf "SAPLING_VERIFY_UPDATE"
    | I_ticket -> fprintf ppf "TICKET"
    | I_read_ticket -> fprintf ppf "READ_TICKET"
    | I_split_ticket -> fprintf ppf "SPLIT_TICKET"
    | I_join_tickets -> fprintf ppf "JOIN_TICKETS"
    | I_dup n -> fprintf ppf "DUP %a" Z.pp_print n
    | I_pair_n n -> fprintf ppf "PAIR %a" Z.pp_print n
    | I_sapling_empty_state n ->
        fprintf ppf "SAPLING_EMPTY_STATE %a" Z.pp_print n
    | I_open_chest -> fprintf ppf "OPEN_CHEST"
    | I_get_and_update_map | I_get_and_update_big_map ->
        fprintf ppf "GET_AND_UPDATE"

  and pp ppf i = pp_inst_t ppf (fst i.Common_adt.Node.value)

  and pp_program fmt { code; param; storage } =
    let open Format in
    let () = fprintf fmt "parameter %a;\n" Typ.pp param in
    let () = fprintf fmt "storage %a;\n" Typ.pp storage in
    fprintf fmt "code { %a }\n" pp code
end
