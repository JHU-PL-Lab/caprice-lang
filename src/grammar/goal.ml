
type t =
  { constraints : bool Formula.t list
  ; assignments : Input_env.t
  ; step : Step.t
  ; priority : Priority.t }

let empty : t =
  { constraints = []
  ; assignments = Input_env.empty
  ; step = Step.dummy
  ; priority = Priority.zero }

let of_solved_target (target : Target.t) model =
  { assignments = Input_env.extend target.i_env ~with_:(Input_env.of_model model)
  ; constraints = target.all_formulas
  ; step = Target.step target
  ; priority = Target.priority target }

let is_before t step =
  Step.compare t.step step < 0

let priority ({ priority ; _ } : t) : Priority.t =
  priority

let step ({ step ; _ } : t) : Step.t =
  step
