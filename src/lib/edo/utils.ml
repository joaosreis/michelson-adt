open! Containers

let for_all2_opt f l1 l2 =
  try Some (List.for_all2 f l1 l2) with Invalid_argument _ -> None
