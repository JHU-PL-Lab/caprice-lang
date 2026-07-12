

type kind =
  | Formula of { cond : bool Formula.t ; do_flip : bool }
  | Tag of { tag : Tag.t ; alternatives : Tag.t list }

type t = { when_ : Step.t ; kind : kind ; logged_inputs : Input_env.t }

let to_priority (t : t) : Priority.t =
  match t.kind with
  | Formula { do_flip ; cond = _ } ->
    if do_flip then Priority.one else Priority.zero
  | Tag { tag ; _ } -> Tag.priority tag
