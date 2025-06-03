open! Containers

type 'a t = { id : int; location : Loc.t; value : 'a } [@@deriving ord, eq]

val create : int -> ?location:Loc.t -> 'a -> 'a t
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
