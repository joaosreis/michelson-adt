open! Containers

type pos = { col : int; lin : int } [@@deriving ord, eq]

type t = { filename : string; start_pos : pos; end_pos : pos }
[@@deriving ord, eq]

let dummy_pos = { col = -1; lin = -1 }
let dummy_loc = { filename = ""; start_pos = dummy_pos; end_pos = dummy_pos }

let to_string l =
  let { filename; start_pos; _ } = l in
  Printf.sprintf "\"%s\", line %d, characters %d" filename start_pos.lin
    start_pos.col

let pp ppf l =
  let { filename; start_pos; end_pos } = l in
  Format.fprintf ppf "{filename: %s; start_pos: (%d, %d); end_pos: (%d, %d)}"
    filename start_pos.lin start_pos.col end_pos.lin end_pos.col

let pp_loc ?max_lines ppf l =
  let open Pp_loc in
  let input = Input.file l.filename in
  let l =
    ( Position.of_line_col l.start_pos.lin l.start_pos.col,
      Position.of_line_col l.end_pos.lin l.end_pos.col )
  in
  match max_lines with
  | None -> pp ~input ppf [ l ]
  | Some max_lines -> pp ~max_lines ~input ppf [ l ]
