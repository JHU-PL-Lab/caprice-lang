(*
  A stem off of a path, represented as a list of path items in reverse order.
*)
type t = { rev_stem : Path_item.t list }

let empty : t = { rev_stem = [] }

let cons (p_item : Path_item.t) (t : t) : t =
  { rev_stem = p_item :: t.rev_stem }

let formulas (t : t) : Formula.BSet.t =
  List.fold_left (fun set item ->
    match item.Path_item.kind with
    | Formula cond
    | Nonflipping cond -> Formula.BSet.add cond set
    | Tag _ -> set
  ) Formula.BSet.empty t.rev_stem

let priority t =
  List.fold_left (fun acc item ->
    Path_priority.plus (Path_item.to_priority item) acc
  ) Path_priority.zero t.rev_stem
