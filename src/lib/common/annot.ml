open! Containers

type t = A_type of string | A_var of string | A_field of string
[@@deriving ord, eq]

let pp ppf = function
  | A_type s -> Format.fprintf ppf ":%s" s
  | A_var s -> Format.fprintf ppf "@%s" s
  | A_field s -> Format.fprintf ppf "%s%s" "%" s
