(*
  A stem off of a goal, represented as a list of path items in reverse order.
  Each path item is associated with the inputs used to get to that point.
*)
type t =
  { rev_stem : (Path_item.t * Input_env.t) list
  ; all_inputs : Input_env.t
  ; goal : Goal.t (* stemming off of this *)
  }

let empty : t =
  { rev_stem = []
  ; all_inputs = Input_env.empty
  ; goal = Goal.empty
  }

let make goal = { empty with goal ; all_inputs = goal.Goal.assignments }

(** [cons p_item t] puts [p_item] as the most recent item in [t] only if the
  goal has been passed. *)
let cons (p_item : Path_item.t) (t : t) : t =
  if Goal.is_before t.goal p_item.when_ then
    { t with rev_stem = (p_item, t.all_inputs) :: t.rev_stem }
  else
    t

(** [log kind key input t] logs the input only if the goal has been passed. *)
let log kind key v t =
  if Goal.is_before t.goal (Stepkey.step key) then
    { t with all_inputs = Input_env.add kind key v t.all_inputs }
  else
    t

let contract t =
  let path_formulas =
    List.fold_left (fun acc (item, _) ->
      match item.Path_item.kind with
      | Formula { cond ; do_flip = _ } -> cond :: acc
      | Tag _ -> acc
    ) t.goal.constraints t.rev_stem
  and path_priority =
    List.fold_left (fun acc (item, _) ->
      Priority.plus (Path_item.priority item) acc
    ) t.goal.priority t.rev_stem
  in
  { Goal.constraints = path_formulas
  ; assignments = t.all_inputs
  ; step = Step.dummy
  ; priority = path_priority }

let forward_stem t =
  List.rev t.rev_stem
