open! Containers

type 'a t = {
  id : int; [@compare fun a b -> Int.compare a b]
  location : Loc.t;
  value : 'a; [@main]
}
[@@deriving ord, eq]

let create id ?(location = Loc.dummy_loc) value = { id; location; value }

let pp pp_value ppf t =
  let { id; location; value } = t in
  Format.fprintf ppf "{ id = %d; location = %a; value = %a }" id Loc.pp location
    pp_value value
