open! Containers

let compare_annot_list a b =
  let f = function Common_adt.Annot.A_type _ -> true | _ -> false in
  if List.is_empty a && List.is_empty b then 0
  else if List.is_empty a && not (List.exists f b) then 0
  else if (not (List.exists f a)) && List.is_empty b then 0
  else
    let f = function Common_adt.Annot.A_type a -> Some a | _ -> None in
    match (List.find_map f a, List.find_map f b) with
    | None, None | None, Some _ | Some _, None -> 0
    | Some a, Some b -> String.compare a b

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
  | Sapling_transaction of Z.t
  | Sapling_state of Z.t
  | Chest
  | Chest_key

and t =
  t'
  * (Common_adt.Annot.t list
    [@compare fun a b -> compare_annot_list a b]
    [@equal fun a b -> compare_annot_list a b = 0])
[@@deriving ord, eq]

let rec is_comparable_type (t, _) =
  match t with
  | Address | Bool | Bytes | Chain_id | Int | Key | Key_hash | Mutez | Nat
  | Never | Unit | String | Signature | Timestamp ->
      true
  | Option t -> is_comparable_type t
  | Or (t_1, t_2) -> is_comparable_type t_1 && is_comparable_type t_2
  | Pair (t_1, t_2) -> is_comparable_type t_1 && is_comparable_type t_2
  | _ -> false

let rec is_packable (t, _) =
  match t with
  | Address | Bls12_381_fr | Bls12_381_g1 | Bls12_381_g2 | Bool | Bytes
  | Contract _ | Int | Key | Key_hash | Lambda _ | Mutez | Nat | Never
  | Sapling_transaction _ | Signature | String | Timestamp | Unit | Chain_id ->
      true
  | Or (t_1, t_2) | Map (t_1, t_2) | Pair (t_1, t_2) ->
      is_packable t_1 && is_packable t_2
  | List t | Option t | Set t -> is_packable t
  | Operation | Chest | Chest_key | Ticket _ | Big_map (_, _) | Sapling_state _
    ->
      false

let rec is_contract_type_compatible contract_t t =
  match (fst contract_t, fst t) with
  | _ when equal contract_t t -> true
  | Pair (contract_1, contract_2), Pair (t_1, t_2) ->
      is_contract_type_compatible contract_1 t_1
      && is_contract_type_compatible contract_2 t_2
  | Or (t_1, t_2), _ ->
      is_contract_type_compatible t_1 t || is_contract_type_compatible t_2 t
  | Contract c, Contract t ->
      is_contract_type_compatible c t || is_contract_type_compatible t c
  | _ -> false

let rec t'_to_string =
  let open Printf in
  function
  | Int -> "int"
  | Nat -> "nat"
  | String -> "string"
  | Bytes -> "bytes"
  | Mutez -> "mutez"
  | Bool -> "bool"
  | Key_hash -> "key_hash"
  | Timestamp -> "timestamp"
  | Address -> "address"
  | Key -> "key"
  | Unit -> "unit"
  | Signature -> "signature"
  | Option t -> sprintf "(option %s)" (to_string t)
  | List t -> sprintf "(list %s)" (to_string t)
  | Set t -> sprintf "(set %s)" (to_string t)
  | Operation -> sprintf "%s" "operation"
  | Contract t -> sprintf "(contract %s)" (to_string t)
  | Pair (t_1, t_2) -> sprintf "(pair %s %s)" (to_string t_1) (to_string t_2)
  | Or (t_1, t_2) -> sprintf "(or %s %s)" (to_string t_1) (to_string t_2)
  | Lambda (t_1, t_2) ->
      sprintf "(lambda %s %s)" (to_string t_1) (to_string t_2)
  | Map (t_1, t_2) -> sprintf "(map %s %s)" (to_string t_1) (to_string t_2)
  | Big_map (t_1, t_2) ->
      sprintf "(big_map %s %s)" (to_string t_1) (to_string t_2)
  | Chain_id -> "chain_id"
  | Never -> "never"
  | Bls12_381_g1 -> "bls12_381_g1"
  | Bls12_381_g2 -> "bls12_381_g2"
  | Bls12_381_fr -> "bls12_381_fr"
  | Ticket t -> sprintf "ticket %s" (to_string t)
  | Sapling_transaction n -> sprintf "sapling_transaction %s" (Z.to_string n)
  | Sapling_state n -> sprintf "sapling_state %s" (Z.to_string n)
  | Chest -> "chest"
  | Chest_key -> "chest_key"

and to_string (t, _) = t'_to_string t

let has_annot a t = List.mem a (snd t) ~eq:Common_adt.Annot.equal

let get_type_annot a =
  List.find_map (function Common_adt.Annot.A_type a -> Some a | _ -> None) a

let rec are_compatible (t_1, a_1) (t_2, a_2) =
  match (get_type_annot a_1, get_type_annot a_2) with
  | Some a_1, Some a_2 when not (String.equal a_1 a_2) -> false
  | _ -> (
      match (t_1, t_2) with
      | Int, Int -> true
      | Nat, Nat -> true
      | String, String -> true
      | Bytes, Bytes -> true
      | Mutez, Mutez -> true
      | Bool, Bool -> true
      | Key_hash, Key_hash -> true
      | Timestamp, Timestamp -> true
      | Address, Address -> true
      | Key, Key -> true
      | Unit, Unit -> true
      | Signature, Signature -> true
      | Option t_1, Option t_2 -> are_compatible t_1 t_2
      | List t_1, List t_2 -> are_compatible t_1 t_2
      | Set t_1, Set t_2 -> are_compatible t_1 t_2
      | Operation, Operation -> true
      | Contract t_1, Contract t_2 -> are_compatible t_1 t_2
      | Pair (t_1_1, t_1_2), Pair (t_2_1, t_2_2) ->
          are_compatible t_1_1 t_2_1 && are_compatible t_1_2 t_2_2
      | Or (t_1_1, t_1_2), Or (t_2_1, t_2_2) ->
          are_compatible t_1_1 t_2_1 && are_compatible t_1_2 t_2_2
      | Lambda (t_1_1, t_1_2), Lambda (t_2_1, t_2_2) ->
          are_compatible t_1_1 t_2_1 && are_compatible t_1_2 t_2_2
      | Map (t_1_1, t_1_2), Map (t_2_1, t_2_2) ->
          are_compatible t_1_1 t_2_1 && are_compatible t_1_2 t_2_2
      | Big_map (t_1_1, t_1_2), Big_map (t_2_1, t_2_2) ->
          are_compatible t_1_1 t_2_1 && are_compatible t_1_2 t_2_2
      | Chain_id, Chain_id -> true
      | Never, Never -> true
      | Bls12_381_g1, Bls12_381_g1 -> true
      | Bls12_381_g2, Bls12_381_g2 -> true
      | Bls12_381_fr, Bls12_381_fr -> true
      | Ticket t_1, Ticket t_2 -> are_compatible t_1 t_2
      | Sapling_transaction n_1, Sapling_transaction n_2 -> Z.equal n_1 n_2
      | Sapling_state n_1, Sapling_state n_2 -> Z.equal n_1 n_2
      | Chest, Chest -> true
      | Chest_key, Chest_key -> true
      | _ -> false)

let rec pp_t' ppf =
  let open Format in
  function
  | Int -> fprintf ppf "int"
  | Nat -> fprintf ppf "nat"
  | String -> fprintf ppf "string"
  | Bytes -> fprintf ppf "bytes"
  | Mutez -> fprintf ppf "mutez"
  | Bool -> fprintf ppf "bool"
  | Key_hash -> fprintf ppf "key_hash"
  | Timestamp -> fprintf ppf "timestamp"
  | Address -> fprintf ppf "address"
  | Key -> fprintf ppf "key"
  | Unit -> fprintf ppf "unit"
  | Signature -> fprintf ppf "signature"
  | Option t -> fprintf ppf "(option %a)" pp t
  | List t -> fprintf ppf "(list %a)" pp t
  | Set t -> fprintf ppf "(set %a)" pp t
  | Operation -> fprintf ppf "%s" "operation"
  | Contract t -> fprintf ppf "(contract %a)" pp t
  | Pair (t_1, t_2) -> fprintf ppf "(pair %a %a)" pp t_1 pp t_2
  | Or (t_1, t_2) -> fprintf ppf "(or %a %a)" pp t_1 pp t_2
  | Lambda (t_1, t_2) -> fprintf ppf "(lambda %a %a)" pp t_1 pp t_2
  | Map (t_1, t_2) -> fprintf ppf "(map %a %a)" pp t_1 pp t_2
  | Big_map (t_1, t_2) -> fprintf ppf "(big_map %a %a)" pp t_1 pp t_2
  | Chain_id -> fprintf ppf "chain_id"
  | Never -> fprintf ppf "never"
  | Bls12_381_g1 -> fprintf ppf "bls12_381_g1"
  | Bls12_381_g2 -> fprintf ppf "bls12_381_g2"
  | Bls12_381_fr -> fprintf ppf "bls12_381_fr"
  | Ticket t -> fprintf ppf "ticket %a" pp t
  | Sapling_transaction n -> fprintf ppf "sapling_transaction %a" Z.pp_print n
  | Sapling_state n -> fprintf ppf "sapling_state %a" Z.pp_print n
  | Chest -> fprintf ppf "chest"
  | Chest_key -> fprintf ppf "chest_key"

and pp ppf (t, a) =
  let open Format in
  let pp_print_list f ppf =
    fprintf ppf "{ %a }"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";") f)
  in
  fprintf ppf "%a %a" pp_t' t (pp_print_list Common_adt.Annot.pp) a
