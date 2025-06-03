open Adt
open Format

let pp_print_list f ppf =
  fprintf ppf "{ %a }" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";") f)

let rec pp_typ ppf (_, t, _) =
  match t with
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
  | T_option t -> fprintf ppf "(option %a)" pp_typ t
  | T_list t -> fprintf ppf "(list %a)" pp_typ t
  | T_set t -> fprintf ppf "(set %a)" pp_typ t
  | T_operation -> fprintf ppf "%s" "operation"
  | T_contract t -> fprintf ppf "(contract %a)" pp_typ t
  | T_pair (t_1, t_2) -> fprintf ppf "(pair %a %a)" pp_typ t_1 pp_typ t_2
  | T_or (t_1, t_2) -> fprintf ppf "(or %a %a)" pp_typ t_1 pp_typ t_2
  | T_lambda (t_1, t_2) -> fprintf ppf "(lambda %a %a)" pp_typ t_1 pp_typ t_2
  | T_map (t_1, t_2) -> fprintf ppf "(map %a %a)" pp_typ t_1 pp_typ t_2
  | T_big_map (t_1, t_2) -> fprintf ppf "(big_map %a %a)" pp_typ t_1 pp_typ t_2
  | T_chain_id -> fprintf ppf "chain_id"

let rec pp_data ppf (_, d) =
  match d with
  | D_int d -> Z.pp_print ppf d
  | D_string s -> fprintf ppf "\"%s\"" s
  | D_bytes b -> fprintf ppf "%s" (Bytes.to_string b)
  | D_elt (d_1, d_2) -> fprintf ppf "Elt %a %a" pp_data d_1 pp_data d_2
  | D_left d -> fprintf ppf "Left %a" pp_data d
  | D_right d -> fprintf ppf "Right %a" pp_data d
  | D_some d -> fprintf ppf "Some %a" pp_data d
  | D_none -> fprintf ppf "None"
  | D_unit -> fprintf ppf "Unit"
  | D_bool b -> fprintf ppf (match b with true -> "True" | false -> "False")
  | D_pair (d_1, d_2) -> fprintf ppf "(Pair %a %a)" pp_data d_1 pp_data d_2
  | D_list d -> pp_print_list pp_data ppf d
  | D_instruction i -> pp_inst ppf i

(* TODO: *)
and pp_inst ppf (_, i, _) =
  match i with
  | I_rename -> fprintf ppf "RENAME"
  | I_abs -> fprintf ppf "ABS"
  | I_drop -> fprintf ppf "DROP"
  | I_dup -> fprintf ppf "DUP"
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
  | I_cast t -> fprintf ppf "CAST %a" pp_typ t
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
  | I_seq i_l -> pp_print_list pp_inst ppf i_l
  | I_drop_n n when n = Z.one -> fprintf ppf "DROP"
  | I_drop_n n -> fprintf ppf "DROP %a" Z.pp_print n
  | I_dig n -> fprintf ppf "DIG %a" Z.pp_print n
  | I_dug n -> fprintf ppf "DUG %a" Z.pp_print n
  | I_push (t, d) -> fprintf ppf "PUSH %a %a" pp_typ t pp_data d
  | I_none t -> fprintf ppf "NONE %a" pp_typ t
  | I_if_none (i_1, i_2) ->
      fprintf ppf "IF_NONE { %a } { %a }" pp_inst i_1 pp_inst i_2
  | I_left t -> fprintf ppf "LEFT %a" pp_typ t
  | I_right t -> fprintf ppf "RIGHT %a" pp_typ t
  | I_if_left (i_1, i_2) ->
      fprintf ppf "IF_LEFT { %a } { %a }" pp_inst i_1 pp_inst i_2
  | I_nil t -> fprintf ppf "NIL %a" pp_typ t
  | I_if_cons (i_1, i_2) ->
      fprintf ppf "IF_CONS { %a } { %a }" pp_inst i_1 pp_inst i_2
  | I_empty_set t -> fprintf ppf "EMPTY_SET %a" pp_typ t
  | I_empty_map (t_1, t_2) ->
      fprintf ppf "EMPTY_MAP %a %a" pp_typ t_1 pp_typ t_2
  | I_empty_big_map (t_1, t_2) ->
      fprintf ppf "EMPTY_BIG_MAP %a %a" pp_typ t_1 pp_typ t_2
  | I_map i -> fprintf ppf "MAP { %a }" pp_inst i
  | I_iter i -> fprintf ppf "ITER { %a }" pp_inst i
  | I_if (i_1, i_2) -> fprintf ppf "IF { %a } { %a }" pp_inst i_1 pp_inst i_2
  | I_loop i -> fprintf ppf "LOOP { %a }" pp_inst i
  | I_loop_left i -> fprintf ppf "LOOP_LEFT { %a }" pp_inst i
  | I_lambda (t_1, t_2, i) ->
      fprintf ppf "LAMBDA %a %a { %a }" pp_typ t_1 pp_typ t_2 pp_inst i
  | I_dip i -> fprintf ppf "DIP { %a }" pp_inst i
  | I_dip_n (n, i) -> fprintf ppf "DIP %a { %a }" Z.pp_print n pp_inst i
  | I_unpack t -> fprintf ppf "UNPACK %a" pp_typ t
  | I_contract t -> fprintf ppf "CONTRACT %a" pp_typ t
  | I_create_contract p -> fprintf ppf "CREATE_CONTRACT { %a }" pp_program p
  | I_apply -> fprintf ppf "APPLY"

and pp_program fmt { code; param; storage } =
  let () = fprintf fmt "parameter %a;\n" pp_typ param in
  let () = fprintf fmt "storage %a;\n" pp_typ storage in
  fprintf fmt "code { %a }\n" pp_inst code
