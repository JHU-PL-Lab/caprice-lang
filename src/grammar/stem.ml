(*
  A stem off of a path, represented as a list of path items in reverse order.
*)
type t = { rev_stem : Path_item.t list }

let empty : t = { rev_stem = [] }

let cons (p_item : Path_item.t) (t : t) : t =
  { rev_stem = p_item :: t.rev_stem }

let formulas (t : t) : bool Formula.t list =
  List.filter_map (fun item ->
    match item.Path_item.kind with
    | Formula { cond ; do_flip = _ } -> Some cond
    | Tag _ -> None
  ) t.rev_stem

let priority t =
  List.fold_left (fun acc item ->
    Priority.plus (Path_item.to_priority item) acc
  ) Priority.zero t.rev_stem
