open! Containers

type pos = { col : int; lin : int } [@@deriving ord]

type t = { filename : string; start_pos : pos; end_pos : pos }
[@@deriving ord, eq]

val dummy_loc : t
val to_string : t -> string
val pp : Format.formatter -> t -> unit
val pp_loc : ?max_lines:int -> Format.formatter -> t -> unit
