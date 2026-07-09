

type kind =
  | Formula of bool Formula.t
  | Nonflipping of bool Formula.t
  | Tag of { tag : Tag.t ; alternatives : Tag.t list }

type t = { when_ : Step.t ; kind : kind ; logged_inputs : Input_env.t }

let to_priority (t : t) : Priority.t =
  match t.kind with
  | Formula _ -> Priority.one
  | Nonflipping _ -> Priority.zero
  | Tag { tag ; _ } -> Tag.priority tag
