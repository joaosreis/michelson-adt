open! Containers

type annot = Common_adt.Annot.t [@@deriving ord, eq]
type 'a node = 'a Common_adt.Node.t [@@deriving ord, eq]

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
  | T_sapling_transaction of Z.t
  | T_sapling_state of Z.t
  | T_chest
  | T_chest_key

and typ = (typ_t * annot list) node [@@deriving ord, eq]

type inst_t =
  | I_noop
  | I_failwith
  | I_seq of inst list
  | I_if of inst * inst
  | I_loop of inst
  | I_loop_left of inst
  | I_dip of inst
  | I_dip_n of Z.t * inst
  | I_exec
  | I_apply
  | I_drop
  | I_drop_n of Z.t
  | I_dup of Z.t
  | I_swap
  | I_dig of Z.t
  | I_dug of Z.t
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
  | I_unpair_n of Z.t
  | I_rename
  | I_total_voting_power
  | I_pairing_check
  | I_sapling_empty_state of Z.t
  | I_sapling_verify_update
  | I_ticket
  | I_read_ticket
  | I_split_ticket
  | I_join_tickets
  | I_never
  | I_self_address
  | I_level
  | I_pair_n of Z.t
  | I_get_n of Z.t
  | I_update_n of Z.t
  | I_open_chest
  | I_get_and_update

and inst = (inst_t * annot list) node [@@deriving ord, eq]

and data_t =
  | D_int of Z.t
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

and data = data_t node [@@deriving ord]
and program = { param : typ; storage : typ; code : inst } [@@deriving ord]

module rec Typ : sig
  type t = typ [@@deriving ord, eq]

  val create :
    int -> ?location:Common_adt.Loc.t -> ?annots:annot list -> typ_t -> t

  val has_annot : Common_adt.Annot.t -> t -> bool
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end = struct
  type t = typ [@@deriving ord, eq]

  let create id ?(location = Common_adt.Loc.dummy_loc) ?(annots = []) t =
    Common_adt.Node.create id ~location (t, annots)

  let has_annot a t =
    List.mem ~eq:Common_adt.Annot.equal a (snd t.Common_adt.Node.value)

  let rec to_string t =
    match fst t.Common_adt.Node.value with
    | T_address -> "address"
    | T_big_map (t1, t2) -> "big_map " ^ to_string t1 ^ " " ^ to_string t2
    | T_bool -> "bool"
    | T_bytes -> "bytes"
    | T_contract t -> "contract " ^ to_string t
    | T_nat -> "nat"
    | T_int -> "int"
    | T_list t -> "list " ^ to_string t
    | T_map (t1, t2) -> "map " ^ to_string t1 ^ " " ^ to_string t2
    | T_or (t1, t2) -> "or " ^ to_string t1 ^ " " ^ to_string t2
    | T_pair (t1, t2) -> "pair " ^ to_string t1 ^ " " ^ to_string t2
    | T_string -> "string"
    | T_unit -> "unit"
    | T_bls12_381_fr -> "bls12_381_fr"
    | T_sapling_state n -> "sapling_state " ^ Z.to_string n
    | T_never -> "never"
    | T_chain_id -> "chain_id"
    | T_mutez -> "mutez"
    | T_key_hash -> "key_hash"
    | T_key -> "key"
    | T_timestamp -> "timestamp"
    | T_signature -> "signature"
    | T_operation -> "operation"
    | T_bls12_381_g1 -> "bls12_381_g1"
    | T_bls12_381_g2 -> "bls12_381_g2"
    | T_chest -> "chest"
    | T_chest_key -> "chest_key"
    | T_option t -> "option " ^ to_string t
    | T_set t -> "set " ^ to_string t
    | T_ticket t -> "ticket " ^ to_string t
    | T_lambda (t_1, t_2) -> "lambda " ^ to_string t_1 ^ " " ^ to_string t_2
    | T_sapling_transaction n -> "sapling_transaction " ^ Z.to_string n

  let rec pp ppf t =
    let open Format in
    match fst t.Common_adt.Node.value with
    | T_int -> fprintf ppf "int"
    | T_nat -> fprintf ppf "nat"
    | T_string -> fprintf ppf "string"
    | T_bytes -> fprintf ppf "bytes"
    | T_mutez -> fprintf ppf "mutez"
    | T_bool -> fprintf ppf "bool"
    | T_key_hash -> fprintf ppf "key_hash"
    | T_timestamp -> fprintf ppf "timestamp"
    | T_address -> fprintf ppf "address"
    | T_key -> fprintf ppf "key"
    | T_unit -> fprintf ppf "unit"
    | T_signature -> fprintf ppf "signature"
    | T_option t -> fprintf ppf "(option %a)" pp t
    | T_list t -> fprintf ppf "(list %a)" pp t
    | T_set t -> fprintf ppf "(set %a)" pp t
    | T_operation -> fprintf ppf "%s" "operation"
    | T_contract t -> fprintf ppf "(contract %a)" pp t
    | T_pair (t_1, t_2) -> fprintf ppf "(pair %a %a)" pp t_1 pp t_2
    | T_or (t_1, t_2) -> fprintf ppf "(or %a %a)" pp t_1 pp t_2
    | T_lambda (t_1, t_2) -> fprintf ppf "(lambda %a %a)" pp t_1 pp t_2
    | T_map (t_1, t_2) -> fprintf ppf "(map %a %a)" pp t_1 pp t_2
    | T_big_map (t_1, t_2) -> fprintf ppf "(big_map %a %a)" pp t_1 pp t_2
    | T_chain_id -> fprintf ppf "chain_id"
    | T_never -> fprintf ppf "never"
    | T_bls12_381_g1 -> fprintf ppf "bls12_381_g1"
    | T_bls12_381_g2 -> fprintf ppf "bls12_381_g2"
    | T_bls12_381_fr -> fprintf ppf "bls12_381_fr"
    | T_ticket t -> fprintf ppf "ticket %a" pp t
    | T_sapling_transaction n ->
        fprintf ppf "sapling_transaction %a" Z.pp_print n
    | T_sapling_state n -> fprintf ppf "sapling_state %a" Z.pp_print n
    | T_chest -> fprintf ppf "chest"
    | T_chest_key -> fprintf ppf "chest_key"
end

and Data : sig
  type t = data [@@deriving ord, eq]

  val create : int -> ?location:Common_adt.Loc.t -> data_t -> t
  val pp : Format.formatter -> t -> unit
end = struct
  type t = data [@@deriving ord, eq]

  let create = Common_adt.Node.create

  let rec pp ppf d =
    let open Format in
    match d.Common_adt.Node.value with
    | D_int d -> Z.pp_print ppf d
    | D_string s -> fprintf ppf "\"%s\"" s
    | D_bytes b -> fprintf ppf "%s" (Bytes.to_string b)
    | D_elt (d_1, d_2) -> fprintf ppf "Elt %a %a" pp d_1 pp d_2
    | D_left d -> fprintf ppf "Left %a" pp d
    | D_right d -> fprintf ppf "Right %a" pp d
    | D_some d -> fprintf ppf "Some %a" pp d
    | D_none -> fprintf ppf "None"
    | D_unit -> fprintf ppf "Unit"
    | D_bool b -> fprintf ppf (match b with true -> "True" | false -> "False")
    | D_pair (d_1, d_2) -> fprintf ppf "(Pair %a %a)" pp d_1 pp d_2
    | D_list d -> pp_print_list pp ppf d
    | D_instruction i -> Inst.pp ppf i
end

and Inst : sig
  type t = inst [@@deriving ord, eq]

  val create :
    int ->
    ?location:Common_adt.Loc.t ->
    ?annots:Common_adt.Annot.t list ->
    inst_t ->
    t

  val pp : Format.formatter -> t -> unit
  val pp_inst_t : Format.formatter -> inst_t -> unit
end = struct
  type t = inst [@@deriving ord, eq]

  let create id ?(location = Common_adt.Loc.dummy_loc) ?(annots = []) inst =
    Common_adt.Node.create id ~location (inst, annots)

  let rec pp_inst_t ppf =
    let open Format in
    function
    | I_rename -> fprintf ppf "RENAME"
    | I_abs -> fprintf ppf "ABS"
    | I_drop -> fprintf ppf "DROP"
    | I_swap -> fprintf ppf "SWAP"
    | I_some -> fprintf ppf "SOME"
    | I_unit -> fprintf ppf "UNIT"
    | I_pair -> fprintf ppf "PAIR"
    | I_car -> fprintf ppf "CAR"
    | I_cdr -> fprintf ppf "CDR"
    | I_cons -> fprintf ppf "CONS"
    | I_size -> fprintf ppf "SIZE"
    | I_mem -> fprintf ppf "MEM"
    | I_get -> fprintf ppf "GET"
    | I_update -> fprintf ppf "UPDATE"
    | I_exec -> fprintf ppf "EXEC"
    | I_failwith -> fprintf ppf "FAILWITH"
    | I_cast t -> fprintf ppf "CAST %a" Typ.pp t
    | I_concat -> fprintf ppf "CONCAT"
    | I_slice -> fprintf ppf "SLICE"
    | I_pack -> fprintf ppf "PACK"
    | I_add -> fprintf ppf "ADD"
    | I_sub -> fprintf ppf "SUB"
    | I_mul -> fprintf ppf "MUL"
    | I_ediv -> fprintf ppf "EDIV"
    | I_isnat -> fprintf ppf "ISNAT"
    | I_int -> fprintf ppf "INT"
    | I_neg -> fprintf ppf "NEG"
    | I_lsl -> fprintf ppf "LSL"
    | I_lsr -> fprintf ppf "LSR"
    | I_or -> fprintf ppf "OR"
    | I_and -> fprintf ppf "AND"
    | I_xor -> fprintf ppf "XOR"
    | I_not -> fprintf ppf "NOT"
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
    | I_unpair -> fprintf ppf "UNPAIR"
    | I_seq _ -> fprintf ppf "" (* TODO: *)
    | I_drop_n n when Z.(equal n one) -> fprintf ppf "DROP"
    | I_drop_n n -> fprintf ppf "DROP %a" Z.pp_print n
    | I_dig n -> fprintf ppf "DIG %a" Z.pp_print n
    | I_dug n -> fprintf ppf "DUG %a" Z.pp_print n
    | I_push (t, d) -> fprintf ppf "PUSH %a %a" Typ.pp t Data.pp d
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
    | I_map i -> fprintf ppf "MAP { %a }" pp i
    | I_iter i -> fprintf ppf "ITER { %a }" pp i
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
    | I_unpair_n n -> fprintf ppf "UNPAIR %a" Z.pp_print n
    | I_get_n n -> fprintf ppf "GET %a" Z.pp_print n
    | I_update_n n -> fprintf ppf "UPDATE %a" Z.pp_print n
    | I_sapling_empty_state n ->
        fprintf ppf "SAPLING_EMPTY_STATE %a" Z.pp_print n
    | I_open_chest -> fprintf ppf "OPEN_CHEST"
    | I_get_and_update -> fprintf ppf "GET_AND_UPDATE"

  and pp ppf i = pp_inst_t ppf (fst i.value)

  and pp_program fmt { code; param; storage } =
    let open Format in
    let () = fprintf fmt "parameter %a;\n" Typ.pp param in
    let () = fprintf fmt "storage %a;\n" Typ.pp storage in
    fprintf fmt "code { %a }\n" pp code
end
