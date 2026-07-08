

type kind =
  | Formula of bool Formula.t
  | Nonflipping of bool Formula.t
  | Tag of { tag : Tag.t ; alternatives : Tag.t list }

type t = { when_ : Step.t ; kind : kind ; logged_inputs : Input_env.t }

let to_priority (t : t) : Path_priority.t =
  match t.kind with
  | Formula _ -> Path_priority.one
  | Nonflipping _ -> Path_priority.zero
  | Tag { tag ; _ } -> Tag.priority tag
