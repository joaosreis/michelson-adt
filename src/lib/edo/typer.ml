open! Core
open Common_adt
open Typ
open Typed_adt

exception Type_error of Loc.t * string
exception Data_error of Adt.Typ.t * Adt.Data.t
exception Failed_error of Loc.t * string
exception Entrypoint_not_found of Loc.t * string

let pop : t Stack.t -> t option = Stack.pop
let push : t Stack.t -> t -> unit = Stack.push
let top : t Stack.t -> t option = Stack.top
let ct t = (t, [])

let raise_invalid_stack_size loc inst =
  raise (Type_error (loc, inst ^ ": invalid stack size"))

let raise_invalid_t loc inst t =
  raise (Type_error (loc, inst ^ ": unexpected type " ^ to_string t))

let raise_invalid_tl loc inst t =
  raise
    (Type_error
       ( loc,
         inst
         ^ List.fold_left t ~init:": unexpected types" ~f:(fun acc t ->
               acc ^ "\n:\r" ^ to_string t) ))

let raise_body_type_mismatch loc inst =
  raise (Type_error (loc, inst ^ ": body stack type mismatch"))

let raise_failed_body loc inst =
  raise (Failed_error (loc, inst ^ ": failed body"))

let raise_entrypoint_not_found loc entrypoint =
  raise (Entrypoint_not_found (loc, entrypoint ^ " entrypoint not found"))

let rec stack_typ_of_typ t =
  let t' =
    match fst t.Node.value with
    | Adt.T_address -> Address
    | T_unit -> Unit
    | T_never -> Never
    | T_bool -> Bool
    | T_int -> Int
    | T_nat -> Nat
    | T_string -> String
    | T_chain_id -> Chain_id
    | T_bytes -> Bytes
    | T_mutez -> Mutez
    | T_key_hash -> Key_hash
    | T_key -> Key
    | T_signature -> Signature
    | T_timestamp -> Timestamp
    | T_operation -> Operation
    | T_bls12_381_g1 -> Bls12_381_g1
    | T_bls12_381_g2 -> Bls12_381_g2
    | T_bls12_381_fr -> Bls12_381_fr
    | T_chest -> Chest
    | T_chest_key -> Chest_key
    | T_option t -> Option (stack_typ_of_typ t)
    | T_list t -> List (stack_typ_of_typ t)
    | T_set t -> Set (stack_typ_of_typ t)
    | T_contract t -> Contract (stack_typ_of_typ t)
    | T_ticket t -> Ticket (stack_typ_of_typ t)
    | T_pair (t_1, t_2) -> Pair (stack_typ_of_typ t_1, stack_typ_of_typ t_2)
    | T_or (t_1, t_2) -> Or (stack_typ_of_typ t_1, stack_typ_of_typ t_2)
    | T_lambda (t_1, t_2) -> Lambda (stack_typ_of_typ t_1, stack_typ_of_typ t_2)
    | T_map (t_1, t_2) -> Map (stack_typ_of_typ t_1, stack_typ_of_typ t_2)
    | T_big_map (t_1, t_2) ->
        Big_map (stack_typ_of_typ t_1, stack_typ_of_typ t_2)
    | T_sapling_transaction n -> Sapling_transaction n
    | T_sapling_state n -> Sapling_state n
  in
  (t', snd t.Node.value)

(* ABS *************)

let type_abs loc stack =
  match pop stack with
  | Some (Int, _) ->
      push stack (ct Nat);
      I_abs
  | None -> raise_invalid_stack_size loc "ABS"
  | Some t -> raise_invalid_t loc "ABS" t

(* DROP *************)

let type_drop loc stack =
  match pop stack with
  | Some _ -> I_drop Bigint.one
  | None -> raise_invalid_stack_size loc "DROP"

(* SWAP *)

let type_swap loc stack =
  match (pop stack, pop stack) with
  | Some x, Some y ->
      push stack x;
      push stack y;
      I_swap
  | _ -> raise_invalid_stack_size loc "SWAP"

(* UNIT *)

let type_unit _loc stack =
  push stack (ct Unit);
  I_unit

(****************************************************************************)

(* EQ *)

let type_eq loc stack =
  match pop stack with
  | Some (Int, _) ->
      push stack (ct Bool);
      I_eq
  | None -> raise_invalid_stack_size loc "EQ"
  | Some t -> raise_invalid_t loc "EQ" t

(* NEQ *)

let type_neq loc stack =
  match pop stack with
  | Some (Int, _) ->
      push stack (ct Bool);
      I_neq
  | None -> raise_invalid_stack_size loc "NEQ"
  | Some t -> raise_invalid_t loc "NEQ" t

(* LT *)

let type_lt loc stack =
  match pop stack with
  | Some (Int, _) ->
      push stack (ct Bool);
      I_lt
  | None -> raise_invalid_stack_size loc "LT"
  | Some t -> raise_invalid_t loc "LT" t

(* GT *)

let type_gt loc stack =
  match pop stack with
  | Some (Int, _) ->
      push stack (ct Bool);
      I_gt
  | None -> raise_invalid_stack_size loc "GT"
  | Some t -> raise_invalid_t loc "GT" t

(* LE *)

let type_le loc stack =
  match pop stack with
  | Some (Int, _) ->
      push stack (ct Bool);
      I_le
  | None -> raise_invalid_stack_size loc "LE"
  | Some t -> raise_invalid_t loc "LE" t

(* GE *)

let type_ge loc stack =
  match pop stack with
  | Some (Int, _) ->
      push stack (ct Bool);
      I_ge
  | None -> raise_invalid_stack_size loc "GE"
  | Some t -> raise_invalid_t loc "GE" t

(* OR *)

let type_or loc stack =
  match (pop stack, pop stack) with
  | Some (Bool, _), Some (Bool, _) ->
      push stack (ct Bool);
      I_or_bool
  | Some (Nat, _), Some (Nat, _) ->
      push stack (ct Nat);
      I_or_nat
  | Some t_1, Some t_2 -> raise_invalid_tl loc "OR" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "OR"

(* AND *)

let type_and loc stack =
  match (pop stack, pop stack) with
  | Some (Bool, _), Some (Bool, _) ->
      push stack (ct Bool);
      I_and_bool
  | Some (Nat, _), Some (Nat, _) ->
      push stack (ct Nat);
      I_and_nat
  | Some (Int, _), Some (Nat, _) ->
      push stack (ct Nat);
      I_and_int_nat
  | Some t_1, Some t_2 -> raise_invalid_tl loc "AND" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "AND"

(* XOR *)

let type_xor loc stack =
  match (pop stack, pop stack) with
  | Some (Bool, _), Some (Bool, _) ->
      push stack (ct Bool);
      I_xor_bool
  | Some (Nat, _), Some (Nat, _) ->
      push stack (ct Nat);
      I_xor_nat
  | Some t_1, Some t_2 -> raise_invalid_tl loc "XOR" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "XOR"

(* NOT *)

let type_not loc stack =
  match pop stack with
  | Some (Bool, _) ->
      push stack (ct Bool);
      I_not_bool
  | Some (Nat, _) ->
      push stack (ct Int);
      I_not_nat
  | Some (Int, _) ->
      push stack (ct Int);
      I_not_int
  | Some t -> raise_invalid_t loc "NOT" t
  | None -> raise_invalid_stack_size loc "NOT"

(* NEG *)

let type_neg loc stack =
  match pop stack with
  | Some (Int, _) ->
      push stack (ct Int);
      I_neg_int
  | Some (Nat, _) ->
      push stack (ct Int);
      I_int_nat
  | Some (Bls12_381_fr, _) ->
      push stack (ct Bls12_381_fr);
      I_neg_bls12_381_fr
  | Some (Bls12_381_g1, _) ->
      push stack (ct Bls12_381_g1);
      I_neg_bls12_381_g1
  | Some (Bls12_381_g2, _) ->
      push stack (ct Bls12_381_g2);
      I_neg_bls12_381_g2
  | Some t -> raise_invalid_t loc "NEG" t
  | None -> raise_invalid_stack_size loc "NEG"

(* ISNAT *)

let type_isnat loc stack =
  match pop stack with
  | Some (Int, _) ->
      push stack (ct (Option (ct Nat)));
      I_isnat
  | Some t -> raise_invalid_t loc "ISNAT" t
  | None -> raise_invalid_stack_size loc "ISNAT"

(****************************************************************************)

(* INT *)

let type_int loc stack =
  match pop stack with
  | Some (Nat, _) ->
      push stack (ct Int);
      I_int_nat
  | Some (Bls12_381_fr, _) ->
      push stack (ct Int);
      I_int_bls12_381_fr
  | Some t -> raise_invalid_t loc "INT" t
  | None -> raise_invalid_stack_size loc "INT"

(* ADD *)

let type_add loc stack =
  match (pop stack, pop stack) with
  | Some (Nat, _), Some (Nat, _) ->
      push stack (ct Nat);
      I_add_nat
  | Some (Nat, _), Some (Int, _) | Some (Int, _), Some (Nat, _) ->
      push stack (ct Int);
      I_add_nat_int
  | Some (Int, _), Some (Int, _) ->
      push stack (ct Int);
      I_add_int
  | Some (Timestamp, _), Some (Int, _) | Some (Int, _), Some (Timestamp, _) ->
      push stack (ct Timestamp);
      I_add_timestamp_int
  | Some (Mutez, _), Some (Mutez, _) ->
      push stack (ct Mutez);
      I_add_mutez
  | Some (Bls12_381_g1, _), Some (Bls12_381_g1, _) ->
      push stack (ct Bls12_381_g1);
      I_add_bls12_381_g1
  | Some (Bls12_381_g2, _), Some (Bls12_381_g2, _) ->
      push stack (ct Bls12_381_g2);
      I_add_bls12_381_g2
  | Some (Bls12_381_fr, _), Some (Bls12_381_fr, _) ->
      push stack (ct Bls12_381_fr);
      I_add_bls12_381_fr
  | Some t_1, Some t_2 -> raise_invalid_tl loc "ADD" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "ADD"

(* SUB *)

let type_sub loc stack =
  match (pop stack, pop stack) with
  | Some (Nat, _), Some (Nat, _) ->
      push stack (ct Int);
      I_sub_nat
  | Some (Nat, _), Some (Int, _) | Some (Int, _), Some (Nat, _) ->
      push stack (ct Int);
      I_sub_nat_int
  | Some (Int, _), Some (Int, _) ->
      push stack (ct Int);
      I_sub_int
  | Some (Timestamp, _), Some (Int, _) ->
      push stack (ct Timestamp);
      I_sub_timestamp_int
  | Some (Timestamp, _), Some (Timestamp, _) ->
      push stack (ct Int);
      I_sub_timestamp
  | Some (Mutez, _), Some (Mutez, _) ->
      push stack (ct Mutez);
      I_sub_mutez
  | Some t_1, Some t_2 -> raise_invalid_tl loc "SUB" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "SUB"

(* MUL *)

let type_mul loc stack =
  match (pop stack, pop stack) with
  | Some (Nat, _), Some (Nat, _) ->
      push stack (ct Nat);
      I_mul_nat
  | Some (Nat, _), Some (Int, _) | Some (Int, _), Some (Nat, _) ->
      push stack (ct Int);
      I_mul_nat_int
  | Some (Int, _), Some (Int, _) ->
      push stack (ct Int);
      I_mul_int
  | Some (Mutez, _), Some (Nat, _) | Some (Nat, _), Some (Mutez, _) ->
      push stack (ct Mutez);
      I_mul_mutez_nat
  | Some (Bls12_381_g1, _), Some (Bls12_381_fr, _) ->
      push stack (ct Bls12_381_g1);
      I_mul_bls12_381_g1_bls12_381_fr
  | Some (Bls12_381_g2, _), Some (Bls12_381_fr, _) ->
      push stack (ct Bls12_381_g2);
      I_mul_bls12_381_g2_bls12_381_fr
  | Some (Bls12_381_fr, _), Some (Bls12_381_fr, _) ->
      push stack (ct Bls12_381_fr);
      I_mul_bls12_381_fr_bls12_381_fr
  | Some (Nat, _), Some (Bls12_381_fr, _) | Some (Bls12_381_fr, _), Some (Nat, _)
    ->
      push stack (ct Bls12_381_fr);
      I_mul_nat_bls12_381_fr
  | Some (Int, _), Some (Bls12_381_fr, _) | Some (Bls12_381_fr, _), Some (Int, _)
    ->
      push stack (ct Bls12_381_fr);
      I_mul_int_bls12_381_fr
  | Some t_1, Some t_2 -> raise_invalid_tl loc "MUL" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "MUL"

(* EDIV *)

let type_ediv loc stack =
  match (pop stack, pop stack) with
  | Some (Nat, _), Some (Nat, _) ->
      push stack (ct (Option (ct (Pair (ct Nat, ct Nat)))));
      I_ediv_nat
  | Some (Nat, _), Some (Int, _) | Some (Int, _), Some (Nat, _) ->
      push stack (ct (Option (ct (Pair (ct Int, ct Nat)))));
      I_ediv_nat_int
  | Some (Int, _), Some (Int, _) ->
      push stack (ct (Option (ct (Pair (ct Int, ct Nat)))));
      I_ediv_int
  | Some (Mutez, _), Some (Nat, _) ->
      push stack (ct (Option (ct (Pair (ct Mutez, ct Mutez)))));
      I_ediv_mutez_nat
  | Some (Mutez, _), Some (Mutez, _) ->
      push stack (ct (Option (ct (Pair (ct Nat, ct Mutez)))));
      I_ediv_mutez
  | Some t_1, Some t_2 -> raise_invalid_tl loc "EDIV" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "EDIV"

(* LSL *)

let type_lsl loc stack =
  match (pop stack, pop stack) with
  | Some (Nat, _), Some (Nat, _) ->
      push stack (ct Nat);
      I_lsl
  | Some t_1, Some t_2 -> raise_invalid_tl loc "LSL" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "LSL"

(* LSR *)

let type_lsr loc stack =
  match (pop stack, pop stack) with
  | Some (Nat, _), Some (Nat, _) ->
      push stack (ct Nat);
      I_lsr
  | Some t_1, Some t_2 -> raise_invalid_tl loc "LSR" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "LSR"

(* COMPARE *)

let type_compare loc stack =
  match (pop stack, pop stack) with
  | Some t_1, Some t_2 when are_compatible t_1 t_2 ->
      if not (is_comparable_type t_1) then
        raise
          (Type_error (loc, "COMPARE: type not comparable " ^ to_string t_1))
      else if not (is_comparable_type t_2) then
        raise
          (Type_error (loc, "COMPARE: type not comparable " ^ to_string t_2))
      else (
        push stack (ct Int);
        I_compare)
  | Some t_1, Some t_2 -> raise_invalid_tl loc "COMPARE" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "COMPARE"

(* CONCAT *)

let type_concat loc stack =
  match pop stack with
  | Some (List (String, _), _) ->
      push stack (ct String);
      I_concat_list_string
  | Some (List (Bytes, _), _) ->
      push stack (ct Bytes);
      I_concat_list_bytes
  | t -> (
      match (t, pop stack) with
      | Some (String, _), Some (String, _) ->
          push stack (ct String);
          I_concat_string
      | Some (Bytes, _), Some (Bytes, _) ->
          push stack (ct Bytes);
          I_concat_bytes
      | None, _ | _, None -> raise_invalid_stack_size loc "CONCAT"
      | Some t_1, Some t_2 -> raise_invalid_tl loc "CONCAT" [ t_1; t_2 ])

(* SIZE *)

let type_size loc stack =
  match pop stack with
  | Some (Set _, _) ->
      push stack (ct Nat);
      I_size_set
  | Some (Map _, _) ->
      push stack (ct Nat);
      I_size_map
  | Some (List _, _) ->
      push stack (ct Nat);
      I_size_list
  | Some (String, _) ->
      push stack (ct Nat);
      I_size_string
  | Some (Bytes, _) ->
      push stack (ct Nat);
      I_size_bytes
  | Some t -> raise_invalid_t loc "SIZE" t
  | None -> raise_invalid_stack_size loc "SIZE"

(* SLICE *)

let type_slice loc stack =
  match (pop stack, pop stack, pop stack) with
  | Some (Nat, _), Some (Nat, _), Some (String, _) ->
      push stack (ct (Option (ct String)));
      I_slice_string
  | Some (Nat, _), Some (Nat, _), Some (Bytes, _) ->
      push stack (ct (Option (ct Bytes)));
      I_slice_bytes
  | Some t_1, Some t_2, Some t_3 ->
      raise_invalid_tl loc "SLICE" [ t_1; t_2; t_3 ]
  | None, _, _ | _, None, _ | _, _, None -> raise_invalid_stack_size loc "SLICE"

(* PAIR *)

let type_pair loc stack =
  match (pop stack, pop stack) with
  | Some t_1, Some t_2 ->
      push stack (ct (Pair (t_1, t_2)));
      I_pair
  | None, _ | _, None -> raise_invalid_stack_size loc "PAIR"

(* CAR *)

let type_car loc stack =
  match pop stack with
  | Some (Pair (t, _), _) ->
      push stack t;
      I_car
  | Some t -> raise_invalid_t loc "CAR" t
  | None -> raise_invalid_stack_size loc "CAR"

(* CDR *)

let type_cdr loc stack =
  match pop stack with
  | Some (Pair (_, t), _) ->
      push stack t;
      I_cdr
  | Some t -> raise_invalid_t loc "CDR" t
  | None -> raise_invalid_stack_size loc "CDR"

(* MEM *)

let type_mem loc stack =
  match (pop stack, pop stack) with
  | Some t, Some (Set t', _) when are_compatible t t' ->
      push stack (ct Bool);
      I_mem_set
  | Some t, Some (Map (t', _), _) when are_compatible t t' ->
      push stack (ct Bool);
      I_mem_map
  | Some t, Some (Big_map (t', _), _) when are_compatible t t' ->
      push stack (ct Bool);
      I_mem_big_map
  | Some t_1, Some t_2 -> raise_invalid_tl loc "MEM" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "MEM"

(****************************************************************)

(* UPDATE *)

let type_update loc stack =
  match (pop stack, pop stack, pop stack) with
  | Some t, Some (Bool, _), Some (Set t', _) when are_compatible t t' ->
      push stack (ct (Set t'));
      I_update_set
  | Some t, Some (Option t_2, _), Some (Map (t', t_2'), _)
    when are_compatible t t' && are_compatible t_2 t_2' ->
      push stack (ct (Map (t', t_2')));
      I_update_map
  | Some t, Some (Option t_2, _), Some (Big_map (t', t_2'), _)
    when are_compatible t t' && are_compatible t_2 t_2' ->
      push stack (ct (Big_map (t', t_2')));
      I_update_map
  | Some t_1, Some t_2, Some t_3 ->
      raise_invalid_tl loc "UPDATE" [ t_1; t_2; t_3 ]
  | None, _, _ | _, None, _ | _, _, None ->
      raise_invalid_stack_size loc "UPDATE"

(* GET *)

let type_get loc stack =
  match (pop stack, pop stack) with
  | Some t_1', Some (Map (t_1, t_2), _) when are_compatible t_1 t_1' ->
      push stack (ct (Option t_2));
      I_get_map
  | Some t_1', Some (Big_map (t_1, t_2), _) when are_compatible t_1 t_1' ->
      push stack (ct (Option t_2));
      I_get_big_map
  | Some t_1, Some t_2 -> raise_invalid_tl loc "GET" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "GET"

(* SOME *)

let type_some loc stack =
  match pop stack with
  | Some t ->
      push stack (ct (Option t));
      I_some
  | None -> raise_invalid_stack_size loc "SOME"

(* NONE *)

let type_none _loc stack t =
  push stack (ct (Option (stack_typ_of_typ t)));
  I_none t

(* CONS *)

let type_cons loc stack =
  match (pop stack, pop stack) with
  | Some t', Some (List t, _) when are_compatible t t' ->
      push stack (ct (List t'));
      I_cons
  | Some t_1, Some t_2 -> raise_invalid_tl loc "CONS" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "CONS"

(* TRANSFER_TOKENS *)

let type_transfer_tokens loc stack =
  match (pop stack, pop stack, pop stack) with
  | Some t, Some (Mutez, _), Some (Contract t', _)
    when is_contract_type_compatible t' t ->
      push stack (ct Operation);
      I_transfer_tokens
  | Some t_1, Some t_2, Some t_3 ->
      raise_invalid_tl loc "TRANSFER_TOKENS" [ t_1; t_2; t_3 ]
  | None, _, _ | _, None, _ | _, _, None ->
      raise_invalid_stack_size loc "TRANSFER_TOKENS"

(****************************************************************)

(* SEDELEGATE *)

let type_set_delegate loc stack =
  match pop stack with
  | Some (Option (Key_hash, _), _) ->
      push stack (ct Operation);
      I_set_delegate
  | Some t -> raise_invalid_t loc "SET_DELEGATE" t
  | None -> raise_invalid_stack_size loc "SET_DELEGATE"

(* BALANCE *)

let type_balance _loc stack =
  push stack (ct Mutez);
  I_balance

(* ADDRESS *)

let type_address loc stack =
  match pop stack with
  | Some (Contract _, _) ->
      push stack (ct Address);
      I_address
  | Some t -> raise_invalid_t loc "ADDRESS" t
  | None -> raise_invalid_stack_size loc "ADDRESS"

(* SOURCE *)

let type_source _loc stack =
  push stack (ct Address);
  I_source

(******************************************************************************)

(* SENDER *)

let type_sender _loc stack =
  push stack (ct Address);
  I_sender

(* SELF *)

let type_self loc stack t annot =
  let t = stack_typ_of_typ t in
  let t =
    match annot with
    | None -> t
    | Some a -> (
        let rec x t =
          let a = Annot.A_field a in
          if has_annot a t then Some t
          else
            match fst t with
            | Or (t_1, t_2) -> (
                if has_annot a t_1 then Some t_1
                else if has_annot a t_2 then Some t_2
                else match x t_1 with None -> x t_2 | Some _ as x -> x)
            | Unit | Never | Bool | Int | Nat | String | Chain_id | Bytes
            | Mutez | Key_hash | Key | Signature | Timestamp | Address
            | Operation | Bls12_381_g1 | Bls12_381_g2 | Bls12_381_fr | Chest
            | Chest_key | Option _ | List _ | Set _ | Contract _ | Ticket _
            | Pair (_, _)
            | Lambda (_, _)
            | Map (_, _)
            | Big_map (_, _)
            | Sapling_transaction _ | Sapling_state _ ->
                None
        in
        match x t with None -> raise_entrypoint_not_found loc a | Some t -> t)
  in
  push stack (ct (Contract t));
  I_self

(* AMOUNT *)

let type_amount _loc stack =
  push stack (ct Mutez);
  I_amount

(* IMPLICIACCOUNT *)

let type_implicit_account loc stack =
  match pop stack with
  | Some (Key_hash, _) ->
      push stack (ct (Contract (ct Unit)));
      I_implicit_account
  | Some t -> raise_invalid_t loc "IMPLICIT_ACCOUNT" t
  | None -> raise_invalid_stack_size loc "IMPLICIT_ACCOUNT"

(* VOTING_POWER *)

let type_voting_power loc stack =
  match pop stack with
  | Some (Key_hash, _) ->
      push stack (ct Nat);
      I_voting_power
  | Some t -> raise_invalid_t loc "VOTING_POWER" t
  | None -> raise_invalid_stack_size loc "VOTING_POWER"

(* NOW *)

let type_now _loc stack =
  push stack (ct Timestamp);
  I_now

(* CHAIN_ID *)

let type_chain_id _loc stack =
  push stack (ct Chain_id);
  I_chain_id

(* PACK *)

let type_pack loc stack =
  match pop stack with
  | Some t ->
      if not (is_packable t) then
        raise (Type_error (loc, "PACK: type is not packable " ^ to_string t))
      else (
        push stack (ct Bytes);
        I_pack)
  | None -> raise_invalid_stack_size loc "PACK"

(* HASH_KEY *)

let type_hash_key loc stack =
  match pop stack with
  | Some (Key, _) ->
      push stack (ct Key_hash);
      I_hash_key
  | None -> raise_invalid_stack_size loc "HASH_KEY"
  | Some t -> raise_invalid_t loc "HASH_KEY" t

(* BLAKE2B *)

let type_blake2b loc stack =
  match pop stack with
  | Some (Bytes, _) ->
      push stack (ct Bytes);
      I_blake2b
  | Some t -> raise_invalid_t loc "BLAKE2B" t
  | None -> raise_invalid_stack_size loc "BLAKE2B"

(* SHA3 *)

let type_sha3 loc stack =
  match pop stack with
  | Some (Bytes, _) ->
      push stack (ct Bytes);
      I_sha3
  | Some t -> raise_invalid_t loc "SHA3" t
  | None -> raise_invalid_stack_size loc "SHA3"

(* SHA256 *)

let type_sha256 loc stack =
  match pop stack with
  | Some (Bytes, _) ->
      push stack (ct Bytes);
      I_sha256
  | Some t -> raise_invalid_t loc "SHA256" t
  | None -> raise_invalid_stack_size loc "SHA256"

(* SHA512 *)

let type_sha512 loc stack =
  match pop stack with
  | Some (Bytes, _) ->
      push stack (ct Bytes);
      I_sha512
  | Some t -> raise_invalid_t loc "SHA512" t
  | None -> raise_invalid_stack_size loc "SHA512"

(* KECCAK *)

let type_keccak loc stack =
  match pop stack with
  | Some (Bytes, _) ->
      push stack (ct Bytes);
      I_keccak
  | Some t -> raise_invalid_t loc "KECCAK" t
  | None -> raise_invalid_stack_size loc "KECCAK"

(* CHECK_SIGNATURE *)

let type_check_signature loc stack =
  match (pop stack, pop stack, pop stack) with
  | Some (Key, _), Some (Signature, _), Some (Bytes, _) ->
      push stack (ct Bool);
      I_check_signature
  | Some t_1, Some t_2, Some t_3 ->
      raise_invalid_tl loc "CHECK_SIGNATURE" [ t_1; t_2; t_3 ]
  | None, _, _ | _, None, _ | _, _, None ->
      raise_invalid_stack_size loc "CHECK_SIGNATURE"

(* TOTAL_VOTING_POWER *)

let type_total_voting_power _loc stack =
  push stack (ct Nat);
  I_total_voting_power

(****************************************************************)

(* PAIRING_CHECK *)

let type_pairing_check loc stack =
  match pop stack with
  | Some (List (Pair ((Bls12_381_g1, _), (Bls12_381_g2, _)), _), _) ->
      push stack (ct Bool);
      I_pairing_check
  | Some t -> raise_invalid_t loc "PAIRING_CHECK" t
  | None -> raise_invalid_stack_size loc "PAIRING_CHECK"

(* SAPLING_VERIFY_UPDATE *)

let type_sapling_verify_update loc stack =
  match (pop stack, pop stack) with
  | Some (Sapling_transaction ms, _), Some (Sapling_state ms', _)
    when Bigint.(ms = ms') ->
      push stack (ct (Option (ct (Pair (ct Int, ct (Sapling_state ms))))));
      I_sapling_verify_update
  | Some t_1, Some t_2 ->
      raise_invalid_tl loc "SAPLING_VERIFY_UPDATE" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "SAPLING_VERIFY_UPDATE"

(* SAPLING_EMPTY_STATE *)

let type_sapling_empty_state _loc stack ms =
  push stack (ct (Sapling_state ms));
  I_sapling_empty_state ms

(* TICKET *)

let type_ticket loc stack =
  match (pop stack, pop stack) with
  | Some t, Some (Nat, _) when is_comparable_type t ->
      push stack (ct (Ticket t));
      I_ticket
  | Some t, Some (Nat, _) -> raise_invalid_t loc "TICKET" t
  | _ -> raise_invalid_stack_size loc "TICKET"

(* READ_TICKET *)

let type_read_ticket loc stack =
  match pop stack with
  | Some (Ticket t, _) ->
      push stack (ct (Ticket t));
      push stack (ct (Pair (ct Address, ct (Pair (t, ct Nat)))));
      I_read_ticket
  | Some t -> raise_invalid_t loc "READ_TICKET" t
  | None -> raise_invalid_stack_size loc "READ_TICKET"

(* SPLIT_TICKET *)

let type_split_ticket loc stack =
  match (pop stack, pop stack) with
  | Some (Ticket t, _), Some (Pair ((Nat, _), (Nat, _)), _) ->
      push stack (ct (Option (ct (Pair (ct (Ticket t), ct (Ticket t))))));
      I_split_ticket
  | Some t_1, Some t_2 -> raise_invalid_tl loc "SPLIT_TICKET" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "SPLIT_TICKET"

(* JOIN_TICKETS *)

let type_join_tickets loc stack =
  match pop stack with
  | Some (Pair ((Ticket t, _), (Ticket t', _)), _) when are_compatible t t' ->
      push stack (ct (Option (ct (Ticket t))));
      I_join_tickets
  | Some t -> raise_invalid_t loc "JOIN_TICKETS" t
  | None -> raise_invalid_stack_size loc "JOIN_TICKETS"

let type_self_address _loc stack =
  push stack (ct Address);
  I_self_address

(* LEVEL *)

let type_level _loc stack =
  push stack (ct Nat);
  I_level

(* OPEN_CHEST *)

let type_open_chest loc stack =
  match (pop stack, pop stack, pop stack) with
  | Some (Chest_key, _), Some (Chest, _), Some (Nat, _) ->
      push stack (ct (Or (ct Bytes, ct Bool)));
      I_open_chest
  | Some t_1, Some t_2, Some t_3 ->
      raise_invalid_tl loc "OPEN_CHEST" [ t_1; t_2; t_3 ]
  | None, _, _ | _, None, _ | _, _, None ->
      raise_invalid_stack_size loc "OPEN_CHEST"

(* GEAND_UPDATE *)

let type_get_and_update loc stack =
  match (pop stack, pop stack, pop stack) with
  | Some k, Some (Option v, _), Some (Map (k', v'), _)
    when are_compatible k k' && are_compatible v v' ->
      push stack (ct (Map (k, v)));
      push stack (ct (Option v));
      I_get_and_update_map
  | Some k, Some (Option v, _), Some (Big_map (k', v'), _)
    when are_compatible k k' && are_compatible v v' ->
      push stack (ct (Big_map (k, v)));
      push stack (ct (Option v));
      I_get_and_update_big_map
  | Some t_1, Some t_2, Some t_3 ->
      raise_invalid_tl loc "GET_AND_UPDATE" [ t_1; t_2; t_3 ]
  | None, _, _ | _, None, _ | _, _, None ->
      raise_invalid_stack_size loc "GET_AND_UPDATE"

(* NIL *)

let type_nil _loc stack t =
  push stack (ct (List (stack_typ_of_typ t)));
  I_nil t

(* EMPTY_SET *)

let type_empty_set _loc stack t =
  push stack (ct (Set (stack_typ_of_typ t)));
  I_empty_set t

(* EMPTY_MAP *)

let type_empty_map _loc stack k v =
  push stack (ct (Map (stack_typ_of_typ k, stack_typ_of_typ v)));
  I_empty_map (k, v)

(* EMPTY_BIG_MAP *)

let type_empty_big_map _loc stack k v =
  push stack (ct (Big_map (stack_typ_of_typ k, stack_typ_of_typ v)));
  I_empty_big_map (k, v)

(* CREATE_CONTRACT *)

let type_create_contract loc stack p =
  match (pop stack, pop stack, pop stack) with
  | Some (Option (Key_hash, _), _), Some (Mutez, _), Some g
    when are_compatible (stack_typ_of_typ p.storage) g ->
      push stack (ct Address);
      push stack (ct Operation);
      I_create_contract p
  | Some t_1, Some t_2, Some t_3 ->
      raise_invalid_tl loc "CREATE_CONTRACT" [ t_1; t_2; t_3 ]
  | None, _, _ | _, None, _ | _, _, None ->
      raise_invalid_stack_size loc "CREATE_CONTRACT"

(* CONTRACT *)

let type_contract loc stack t =
  match pop stack with
  | Some (Address, _) ->
      push stack (ct (Option (ct (Contract (stack_typ_of_typ t)))));
      I_contract t
  | Some t -> raise_invalid_t loc "CONTRACT" t
  | None -> raise_invalid_stack_size loc "CONTRACT"

(****************************************************************************)

(* UNPACK *)

let type_unpack loc stack t =
  match pop stack with
  | Some (Bytes, _) ->
      push stack (ct (Option (stack_typ_of_typ t)));
      I_unpack t
  | Some t -> raise_invalid_t loc "UNPACK" t
  | None -> raise_invalid_stack_size loc "UNPACK"

(* CAST *)

let type_cast loc stack t =
  let t'' = stack_typ_of_typ t in
  match pop stack with
  | Some t' when are_compatible t' t'' ->
      push stack t'';
      I_cast t
  | Some t -> raise_invalid_t loc "CAST" t
  | None -> raise_invalid_stack_size loc "CAST"

(* CREATE_ACCOUNT *)

let type_create_account loc stack =
  match (pop stack, pop stack, pop stack, pop stack) with
  | ( Some (Key_hash, _),
      Some (Option (Key_hash, _), _),
      Some (Bool, _),
      Some (Mutez, _) ) ->
      push stack (ct Address);
      push stack (ct Operation);
      I_create_account
  | Some t_1, Some t_2, Some t_3, Some t_4 ->
      raise_invalid_tl loc "CREATE_ACCOUNT" [ t_1; t_2; t_3; t_4 ]
  | None, _, _, _ | _, None, _, _ | _, _, None, _ | _, _, _, None ->
      raise_invalid_stack_size loc "CREATE_ACCOUNT"

(****************************************************************************)

(* EXEC *)

let type_exec loc stack =
  match (pop stack, pop stack) with
  | Some a, Some (Lambda (a', b), _) when are_compatible a a' ->
      push stack b;
      I_exec
  | Some t_1, Some t_2 -> raise_invalid_tl loc "EXEC" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "EXEC"

(* APPLY *)

let type_apply loc stack =
  match (pop stack, pop stack) with
  | Some a, Some (Lambda ((Pair (a', b), _), c), _) when are_compatible a a' ->
      push stack (ct (Lambda (b, c)));
      I_apply
  | Some t_1, Some t_2 -> raise_invalid_tl loc "APPLY" [ t_1; t_2 ]
  | None, _ | _, None -> raise_invalid_stack_size loc "APPLY"

(* DROP n *)

let type_drop_n loc stack n =
  let rec aux i =
    if Bigint.(i = n) then ()
    else
      match pop stack with
      | Some _ -> aux Bigint.(i + one)
      | None -> raise_invalid_stack_size loc "DROP"
  in
  aux Bigint.zero;
  I_drop n

(* push *)

let type_push _loc stack d =
  push stack (fst d.Node.value |> stack_typ_of_typ);
  I_push d

(****************************************************************)

(* LEFT *)

let type_left loc stack t =
  match pop stack with
  | Some left ->
      push stack (ct (Or (left, stack_typ_of_typ t)));
      I_left t
  | None -> raise_invalid_stack_size loc "LEFT"

(* RIGHT *)

let type_right loc stack t =
  match pop stack with
  | Some right ->
      push stack (ct (Or (stack_typ_of_typ t, right)));
      I_right t
  | None -> raise_invalid_stack_size loc "RIGHT"

(* UNPAIR n *)

let type_unpair loc stack n =
  let rec aux i x =
    match pop stack with
    | Some (Pair (t_1, t_2), _) ->
        if Bigint.(i = of_int 2) then (
          push stack t_2;
          push stack t_1;
          List.iter x ~f:(fun x -> push stack x))
        else (
          push stack t_2;
          aux Bigint.(i - one) (t_1 :: x))
    | Some t -> raise_invalid_t loc "UNPAIR" t
    | None -> raise_invalid_stack_size loc "UNPAIR"
  in
  aux n [];
  I_unpair n

(* DUP n *)

let type_dup loc stack n =
  let rec aux i x =
    match pop stack with
    | Some t ->
        if Bigint.(i = one) then (
          push stack t;
          List.iter x ~f:(push stack);
          push stack t)
        else aux Bigint.(i - one) (t :: x)
    | None -> raise_invalid_stack_size loc "DUP"
  in
  aux n [];
  I_dup n

(* DIG *)

let type_dig loc stack n =
  let rec aux i x =
    match pop stack with
    | Some t ->
        if Bigint.(i = zero) then (
          List.iter x ~f:(fun x -> push stack x);
          push stack t)
        else aux Bigint.(i - one) (t :: x)
    | None -> raise_invalid_stack_size loc "DIG"
  in
  aux n [];
  I_dig n

(* DUG n *)

let type_dug loc stack n =
  (if Bigint.(n = zero) then ()
  else
    match pop stack with
    | Some top ->
        let rec aux i x =
          match pop stack with
          | Some a ->
              if Bigint.(i = one) then (
                push stack top;
                push stack a;
                List.iter x ~f:(fun x -> push stack x))
              else aux Bigint.(i - one) (a :: x)
          | None -> raise_invalid_stack_size loc "DUG"
        in
        aux n []
    | None -> raise_invalid_stack_size loc "DUG");
  I_dug n

(* PAIR n *)

let type_pair_n loc stack n =
  let rec aux i l =
    if Bigint.(i = of_int 2) then
      match (pop stack, pop stack) with
      | Some t_1, Some t_2 ->
          let x = ct (Pair (t_1, t_2)) in
          let x =
            List.fold_left l ~init:x ~f:(fun acc x -> ct (Pair (x, acc)))
          in
          push stack x
      | None, _ | _, None -> raise_invalid_stack_size loc "PAIR"
    else
      match pop stack with
      | Some x -> aux Bigint.(i - one) (x :: l)
      | None -> raise_invalid_stack_size loc "PAIR"
  in
  aux n [];
  I_pair_n n

(* GET n *)

let type_get_n loc stack n =
  let rec aux i =
    if Bigint.(i = zero) then ()
    else if Bigint.(i = one) then
      match pop stack with
      | Some (Pair (a, _), _) -> push stack a
      | Some t -> raise_invalid_t loc "GET" t
      | None -> raise_invalid_stack_size loc "GET"
    else
      match pop stack with
      | Some (Pair (_, y), _) ->
          push stack y;
          aux Bigint.(i - one - one)
      | Some t -> raise_invalid_t loc "GET" t
      | None -> raise_invalid_stack_size loc "GET"
  in
  aux n;
  I_get_n n

(* UPDATE n *)

let type_update_n loc stack n =
  match pop stack with
  | Some a -> (
      let rec aux i f x =
        if Bigint.(i = zero) then f a
        else if Bigint.(i = one) then
          match x with
          | Pair (_, b), _ -> f (ct (Pair (a, b)))
          | t -> raise_invalid_t loc "UPDATE" t
        else
          match x with
          | Pair (a, b), _ ->
              let f x = f (ct (Pair (a, x))) in
              aux Bigint.(i - one - one) f b
          | t -> raise_invalid_t loc "UPDATE" t
      in
      match pop stack with
      | Some t ->
          let s = aux n (fun x -> x) t in
          push stack s;
          I_update_n n
      | None -> raise_invalid_stack_size loc "UPDATE")
  | _ -> raise_invalid_stack_size loc "UPDATE"

(****************************************************************)
let rec type_data t d =
  let raise_invalid_data () = raise (Data_error (t, d)) in
  let d' =
    match (fst t.Node.value, d.Node.value) with
    | Adt.T_bool, Adt.D_bool b -> D_bool b
    | (T_int | T_nat | T_timestamp | T_mutez | T_bls12_381_fr), D_int i ->
        D_int i
    | ( ( T_bytes | T_bls12_381_g1 | T_bls12_381_g2 | T_bls12_381_fr | T_address
        | T_key ),
        D_bytes b ) ->
        D_bytes b
    | ( ( T_string | T_timestamp | T_contract _ | T_address | T_key_hash
        | T_signature | T_key ),
        D_string s ) ->
        D_string s
    | T_unit, D_unit -> D_unit
    | T_option _, D_none -> D_none
    | T_option t', D_some d ->
        let d = type_data t' d in
        D_some d
    | T_pair (t_1, t_2), D_pair (d_1, d_2) ->
        let d_1 = type_data t_1 d_1 in
        let d_2 = type_data t_2 d_2 in
        D_pair (d_1, d_2)
    | T_ticket t, D_pair (a, v) ->
        let d = type_data t v in
        let a = type_data (Typ.create 0 T_address) a in
        D_pair (a, d)
    | T_list t', D_list d_l ->
        let d = List.rev_map d_l ~f:(type_data t') in
        D_list (List.rev d)
    | T_set t', D_list d_l ->
        let d = List.rev_map d_l ~f:(type_data t') in
        D_list (List.rev d)
    | T_map (t_k, t_v), D_list d_l ->
        let type_elt t_k t_v d =
          match d.Node.value with
          | Adt.D_elt (d_k, d_v) ->
              let d_k = type_data t_k d_k in
              let d_v = type_data t_v d_v in
              (d_k, d_v)
          | _ -> raise_invalid_data ()
        in
        let d_l = List.rev_map d_l ~f:(type_elt t_k t_v) in
        D_map (List.rev d_l)
    | T_or (t', _), D_left d ->
        let d = type_data t' d in
        D_left d
    | T_or (_, t'), D_right d ->
        let d = type_data t' d in
        D_right d
    | T_lambda (p, r), D_instruction i -> (
        match type_lambda d.location (Stack.create ()) p r i with
        | I_lambda (_, _, i) -> D_instruction i
        | _ -> raise_invalid_data ())
    | _ ->
        (* To debug: *)
        (* Stdio.print_endline (Sexp.to_string (Typed_adt.sexp_of_typ t));
           Stdio.print_endline (Sexp.to_string ([%sexp_of: _ Adt.data_t] d)); *)
        raise_invalid_data ()
  in
  { d with value = (t, d') }

and type_if loc p stack i_t i_f =
  match pop stack with
  | Some (Bool, _) -> (
      let s_t = Stack.copy stack in
      let s_f = Stack.copy stack in
      let failed_t, i_t = type_inst p s_t i_t in
      let failed_f, i_f = type_inst p s_f i_f in
      let copy_stack () =
        if not failed_t then (
          Stack.clear stack;
          let l = Stack.fold s_t ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(push stack))
        else if not failed_f then (
          Stack.clear stack;
          let l = Stack.fold s_f ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(push stack))
      in
      let failed = failed_t && failed_f in
      let i = (failed, I_if (i_t, i_f)) in
      if failed then i
      else if failed_t || failed_f then (
        copy_stack ();
        i)
      else
        let l_s_t = Stack.to_list s_t in
        let l_s_f = Stack.to_list s_f in
        match List.for_all2 l_s_t l_s_f ~f:are_compatible with
        | List.Or_unequal_lengths.Ok b when b ->
            copy_stack ();
            i
        | _ -> raise_body_type_mismatch loc "IF")
  | Some t -> raise_invalid_t loc "IF" t
  | None -> raise_invalid_stack_size loc "IF"

and type_if_none loc p stack i_t i_f =
  match pop stack with
  | Some (Option t, _) -> (
      let s_t = Stack.copy stack in
      let s_f = Stack.copy stack in
      push s_f t;
      let failed_t, i_t = type_inst p s_t i_t in
      let failed_f, i_f = type_inst p s_f i_f in
      let copy_stack () =
        if not failed_t then (
          Stack.clear stack;
          let l = Stack.fold s_t ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(push stack))
        else if not failed_f then (
          Stack.clear stack;
          let l = Stack.fold s_f ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(push stack))
      in
      let failed = failed_t && failed_f in
      let i = (failed, I_if_none (i_t, i_f)) in
      if failed then i
      else if failed_t || failed_f then (
        copy_stack ();
        i)
      else
        let l_s_t = Stack.to_list s_t in
        let l_s_f = Stack.to_list s_f in
        match List.for_all2 l_s_t l_s_f ~f:are_compatible with
        | List.Or_unequal_lengths.Ok b when b ->
            copy_stack ();
            i
        | _ -> raise_body_type_mismatch loc "IF_NONE")
  | Some t -> raise_invalid_t loc "IF_NONE" t
  | None -> raise_invalid_stack_size loc "IF_NONE"

and type_if_left loc p stack i_t i_f =
  match pop stack with
  | Some (Or (t, f), _) -> (
      let s_t = Stack.copy stack in
      let s_f = Stack.copy stack in
      push s_t t;
      push s_f f;
      let failed_t, i_t = type_inst p s_t i_t in
      let failed_f, i_f = type_inst p s_f i_f in
      let copy_stack () =
        if not failed_t then (
          Stack.clear stack;
          let l = Stack.fold s_t ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(push stack))
        else if not failed_f then (
          Stack.clear stack;
          let l = Stack.fold s_f ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(push stack))
      in
      let failed = failed_t && failed_f in
      let i = (failed, I_if_left (i_t, i_f)) in
      if failed then i
      else if failed_t || failed_f then (
        copy_stack ();
        i)
      else
        let l_s_t = Stack.to_list s_t in
        let l_s_f = Stack.to_list s_f in
        match List.for_all2 l_s_t l_s_f ~f:are_compatible with
        | List.Or_unequal_lengths.Ok b when b ->
            copy_stack ();
            i
        | _ -> raise_body_type_mismatch loc "IF_LEFT")
  | Some t -> raise_invalid_t loc "IF_LEFT" t
  | None -> raise_invalid_stack_size loc "IF_LEFT"

and type_if_cons loc p stack i_t i_f =
  match pop stack with
  | Some (List t, _) -> (
      let s_t = Stack.copy stack in
      let s_f = Stack.copy stack in
      push s_t (ct (List t));
      push s_t t;
      let failed_t, i_t = type_inst p s_t i_t in
      let failed_f, i_f = type_inst p s_f i_f in
      let copy_stack () =
        if not failed_t then (
          Stack.clear stack;
          let l = Stack.fold s_t ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(push stack))
        else if not failed_f then (
          Stack.clear stack;
          let l = Stack.fold s_f ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(push stack))
      in
      let failed = failed_t && failed_f in
      let i = (failed, I_if_cons (i_t, i_f)) in
      if failed then i
      else if failed_t || failed_f then (
        copy_stack ();
        i)
      else
        let l_s_t = Stack.to_list s_t in
        let l_s_f = Stack.to_list s_f in
        match List.for_all2 l_s_t l_s_f ~f:are_compatible with
        | List.Or_unequal_lengths.Ok b when b ->
            copy_stack ();
            i
        | _ -> raise_body_type_mismatch loc "IF_CONS")
  | Some t -> raise_invalid_t loc "IF_CONS" t
  | None -> raise_invalid_stack_size loc "IF_CONS"

and type_loop loc p stack b =
  match pop stack with
  | Some (Bool, _) -> (
      let s = Stack.copy stack in
      let failed, b = type_inst p s b in
      let i = (failed, I_loop b) in
      if failed then i
      else
        match pop s with
        | Some (Bool, _) -> (
            let l_stack = Stack.to_list stack in
            let l_s = Stack.to_list s in
            match List.for_all2 l_stack l_s ~f:are_compatible with
            | List.Or_unequal_lengths.Ok c when c -> i
            | _ -> raise_body_type_mismatch loc "LOOP")
        | Some t -> raise_invalid_t loc "LOOP" t
        | None -> raise_invalid_stack_size loc "LOOP")
  | Some t -> raise_invalid_t loc "LOOP" t
  | None -> raise_invalid_stack_size loc "LOOP"

and type_loop_left loc p stack b =
  match pop stack with
  | Some (Or (l, r), _) -> (
      let s = Stack.copy stack in
      push s l;
      let failed, b = type_inst p s b in
      let i = (failed, I_loop_left b) in
      if failed then i
      else
        match pop s with
        | Some (Or (l', r'), _) when are_compatible l l' && are_compatible r r'
          -> (
            let l_stack = Stack.to_list stack in
            let l_s = Stack.to_list s in
            match List.for_all2 l_stack l_s ~f:are_compatible with
            | List.Or_unequal_lengths.Ok c when c ->
                push stack r;
                i
            | _ -> raise_body_type_mismatch loc "LOOP_LEFT")
        | Some t -> raise_invalid_t loc "LOOP_LEFT" t
        | None -> raise_invalid_stack_size loc "LOOP_LEFT")
  | Some t -> raise_invalid_t loc "LOOP_LEFT" t
  | None -> raise_invalid_stack_size loc "LOOP_LEFT"

and type_iter loc p stack b =
  match pop stack with
  | Some (Set t, _) -> (
      let s = Stack.copy stack in
      push s t;
      let failed, b = type_inst p s b in
      let i = (failed, I_iter_set b) in
      if failed then i
      else
        let l_stack = Stack.to_list stack in
        let l_s = Stack.to_list s in
        match List.for_all2 l_stack l_s ~f:are_compatible with
        | List.Or_unequal_lengths.Ok c when c -> i
        | _ -> raise_body_type_mismatch loc "ITER")
  | Some (Map (k, v), _) -> (
      let s = Stack.copy stack in
      push s (ct (Pair (k, v)));
      let failed, b = type_inst p s b in
      let i = (failed, I_iter_map b) in
      if failed then i
      else
        let l_stack = Stack.to_list stack in
        let l_s = Stack.to_list s in
        match List.for_all2 l_stack l_s ~f:are_compatible with
        | List.Or_unequal_lengths.Ok c when c -> i
        | _ -> raise_body_type_mismatch loc "ITER")
  | Some (List t, _) -> (
      let s = Stack.copy stack in
      push s t;
      let failed, b = type_inst p s b in
      let i = (failed, I_iter_list b) in
      if failed then i
      else
        let l_stack = Stack.to_list stack in
        let l_s = Stack.to_list s in
        match List.for_all2 l_stack l_s ~f:are_compatible with
        | List.Or_unequal_lengths.Ok c when c -> i
        | _ -> raise_body_type_mismatch loc "ITER")
  | Some t -> raise_invalid_t loc "ITER" t
  | None -> raise_invalid_stack_size loc "ITER"

and type_map loc p stack b =
  match pop stack with
  | Some (List t, _) -> (
      let s = Stack.copy stack in
      push s t;
      let failed, b = type_inst p s b in
      if failed then raise_failed_body loc "MAP"
      else
        let i = (false, I_map_list b) in
        match pop s with
        | Some t -> (
            let l_stack = Stack.to_list stack in
            let l_s = Stack.to_list s in
            match List.for_all2 l_stack l_s ~f:are_compatible with
            | List.Or_unequal_lengths.Ok c when c ->
                push stack (ct (List t));
                i
            | _ -> raise_body_type_mismatch loc "MAP")
        | None -> raise_invalid_stack_size loc "MAP")
  | Some (Map (k, v), _) -> (
      let s = Stack.copy stack in
      push s (ct (Pair (k, v)));
      let failed, b = type_inst p s b in
      if failed then raise_failed_body loc "MAP"
      else
        let i = (false, I_map_map b) in
        match pop s with
        | Some t -> (
            let l_stack = Stack.to_list stack in
            let l_s = Stack.to_list s in
            match List.for_all2 l_stack l_s ~f:are_compatible with
            | List.Or_unequal_lengths.Ok c when c ->
                push stack (ct (Map (k, t)));
                i
            | _ -> raise_body_type_mismatch loc "MAP")
        | None -> raise_invalid_stack_size loc "MAP")
  | Some t -> raise_invalid_t loc "MAP" t
  | None -> raise_invalid_stack_size loc "MAP"

and type_lambda loc stack p r b =
  let s = Stack.create () in
  let p' = stack_typ_of_typ p in
  let r' = stack_typ_of_typ r in
  push s p';
  let failed, b = type_inst p s b in
  let i = I_lambda (p, r, b) in
  push stack (ct (Lambda (p', r')));
  if failed then i
  else
    match pop s with
    | Some r'' when are_compatible r' r'' && Stack.is_empty s -> i
    | Some t -> raise_invalid_t loc "LAMBDA" t
    | None -> raise_invalid_stack_size loc "LAMBDA"

and type_dip loc p stack i =
  match pop stack with
  | Some t ->
      let failed, i = type_inst p stack i in
      if failed then raise_failed_body loc "DIP"
      else
        let i = (false, I_dip i) in
        push stack t;
        i
  | None -> raise_invalid_stack_size loc "DIP"

and type_dip_n loc p stack n i =
  if Bigint.(of_int (Stack.length stack) < n) then
    raise (Invalid_argument "DIP n")
  else
    let l =
      List.init (Bigint.to_int_exn n) ~f:(fun _ ->
          match pop stack with
          | Some x -> x
          | None -> raise_invalid_stack_size loc "DIP n")
    in
    let failed, i = type_inst p stack i in
    if failed then raise_failed_body loc "DIP n"
    else
      let i = (false, I_dip_n (n, i)) in
      List.iter l ~f:(push stack);
      i

and type_seq p stack i_l =
  let type_inst = type_inst p stack in
  match i_l with
  | [] -> (false, I_noop)
  | x :: xs ->
      let failed, x = type_inst x in
      let failed, seq =
        List.fold_left xs ~init:(failed, [ x ]) ~f:(fun (failed, acc) ->
          function
          | i ->
              if failed then raise_failed_body i.location "MAP"
              else
                let failed, i = type_inst i in
                (failed, i :: acc))
      in
      (failed, I_seq (List.rev seq))

and type_program p =
  let s = Stack.create () in
  let param = p.Adt.param in
  let storage = p.Adt.storage in
  push s (ct (Pair (stack_typ_of_typ param, stack_typ_of_typ storage)));
  let _, code = type_inst param s p.Adt.code in
  { param; storage; code }

and type_inst p stack { id; location = loc; value = i, annots } =
  let v, i =
    match i with
    | Adt.I_abs -> (false, type_abs loc stack)
    | I_noop -> (false, I_noop)
    | I_drop -> (false, type_drop loc stack)
    | I_failwith -> (true, I_failwith)
    | I_swap -> (false, type_swap loc stack)
    | I_unit -> (false, type_unit loc stack)
    | I_eq -> (false, type_eq loc stack)
    | I_neq -> (false, type_neq loc stack)
    | I_lt -> (false, type_lt loc stack)
    | I_gt -> (false, type_gt loc stack)
    | I_le -> (false, type_le loc stack)
    | I_ge -> (false, type_ge loc stack)
    | I_or -> (false, type_or loc stack)
    | I_and -> (false, type_and loc stack)
    | I_xor -> (false, type_xor loc stack)
    | I_not -> (false, type_not loc stack)
    | I_neg -> (false, type_neg loc stack)
    | I_isnat -> (false, type_isnat loc stack)
    | I_int -> (false, type_int loc stack)
    | I_add -> (false, type_add loc stack)
    | I_sub -> (false, type_sub loc stack)
    | I_mul -> (false, type_mul loc stack)
    | I_ediv -> (false, type_ediv loc stack)
    | I_lsl -> (false, type_lsl loc stack)
    | I_lsr -> (false, type_lsr loc stack)
    | I_compare -> (false, type_compare loc stack)
    | I_concat -> (false, type_concat loc stack)
    | I_size -> (false, type_size loc stack)
    | I_slice -> (false, type_slice loc stack)
    | I_pair -> (false, type_pair loc stack)
    | I_car -> (false, type_car loc stack)
    | I_cdr -> (false, type_cdr loc stack)
    | I_mem -> (false, type_mem loc stack)
    | I_update -> (false, type_update loc stack)
    | I_get -> (false, type_get loc stack)
    | I_some -> (false, type_some loc stack)
    | I_none t -> (false, type_none loc stack t)
    | I_cons -> (false, type_cons loc stack)
    | I_transfer_tokens -> (false, type_transfer_tokens loc stack)
    | I_set_delegate -> (false, type_set_delegate loc stack)
    | I_balance -> (false, type_balance loc stack)
    | I_address -> (false, type_address loc stack)
    | I_source -> (false, type_source loc stack)
    | I_sender -> (false, type_sender loc stack)
    | I_self ->
        ( false,
          type_self loc stack p
            (List.find_map annots ~f:(function
              | Annot.A_field a -> Some a
              | _ -> None)) )
    | I_amount -> (false, type_amount loc stack)
    | I_implicit_account -> (false, type_implicit_account loc stack)
    | I_voting_power -> (false, type_voting_power loc stack)
    | I_now -> (false, type_now loc stack)
    | I_chain_id -> (false, type_chain_id loc stack)
    | I_pack -> (false, type_pack loc stack)
    | I_hash_key -> (false, type_hash_key loc stack)
    | I_blake2b -> (false, type_blake2b loc stack)
    | I_keccak -> (false, type_keccak loc stack)
    | I_sha3 -> (false, type_sha3 loc stack)
    | I_sha256 -> (false, type_sha256 loc stack)
    | I_sha512 -> (false, type_sha512 loc stack)
    | I_check_signature -> (false, type_check_signature loc stack)
    | I_unpair -> (false, type_unpair loc stack Bigint.(one + one))
    | I_total_voting_power -> (false, type_total_voting_power loc stack)
    | I_pairing_check -> (false, type_pairing_check loc stack)
    | I_sapling_verify_update -> (false, type_sapling_verify_update loc stack)
    | I_ticket -> (false, type_ticket loc stack)
    | I_read_ticket -> (false, type_read_ticket loc stack)
    | I_split_ticket -> (false, type_split_ticket loc stack)
    | I_join_tickets -> (false, type_join_tickets loc stack)
    | I_never -> (true, I_never)
    | I_self_address -> (false, type_self_address loc stack)
    | I_level -> (false, type_level loc stack)
    | I_open_chest -> (false, type_open_chest loc stack)
    | I_get_and_update -> (false, type_get_and_update loc stack)
    | I_seq i_l -> type_seq p stack i_l
    | I_nil t -> (false, type_nil loc stack t)
    | I_empty_set t -> (false, type_empty_set loc stack t)
    | I_empty_map (k, v) -> (false, type_empty_map loc stack k v)
    | I_empty_big_map (k, v) -> (false, type_empty_big_map loc stack k v)
    | I_create_contract p ->
        (false, type_create_contract loc stack (type_program p))
    | I_contract t -> (false, type_contract loc stack t)
    | I_unpack t -> (false, type_unpack loc stack t)
    | I_cast t -> (false, type_cast loc stack t)
    | I_sapling_empty_state ms -> (false, type_sapling_empty_state loc stack ms)
    | I_create_account -> (false, type_create_account loc stack)
    | I_if (i_t, i_f) -> type_if loc p stack i_t i_f
    | I_loop b -> type_loop loc p stack b
    | I_dip i -> type_dip loc p stack i
    | I_exec -> (false, type_exec loc stack)
    | I_apply -> (false, type_apply loc stack)
    | I_drop_n n -> (false, type_drop_n loc stack n)
    | I_push (t, d) -> (false, type_push loc stack (type_data t d))
    | I_left t -> (false, type_left loc stack t)
    | I_right t -> (false, type_right loc stack t)
    | I_unpair_n n -> (false, type_unpair loc stack n)
    | I_dip_n (n, i) -> type_dip_n loc p stack n i
    | I_dup n -> (false, type_dup loc stack n)
    | I_dig n -> (false, type_dig loc stack n)
    | I_dug n -> (false, type_dug loc stack n)
    | I_pair_n n -> (false, type_pair_n loc stack n)
    | I_get_n n -> (false, type_get_n loc stack n)
    | I_update_n n -> (false, type_update_n loc stack n)
    | I_if_none (i_t, i_f) -> type_if_none loc p stack i_t i_f
    | I_if_left (i_t, i_f) -> type_if_left loc p stack i_t i_f
    | I_if_cons (i_t, i_f) -> type_if_cons loc p stack i_t i_f
    | I_loop_left b -> type_loop_left loc p stack b
    | I_lambda (p, r, b) -> (false, type_lambda loc stack p r b)
    | I_iter b -> type_iter loc p stack b
    | I_map b -> type_map loc p stack b
    | I_rename -> (false, I_noop)
  in
  (v, { id; location = loc; value = (i, annots) })
