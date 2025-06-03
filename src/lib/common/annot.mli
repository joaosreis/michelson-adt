open! Containers

type t = A_type of string | A_var of string | A_field of string
[@@deriving ord, eq]

val pp : Format.formatter -> t -> unit
