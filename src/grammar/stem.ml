(*
  A stem off of a target, represented as a list of path items in reverse order.
  Each path item is associated with the inputs used to get to that point.
*)
type t =
  { rev_stem : (Path_item.t * Input_env.t) list
  ; all_inputs : Input_env.t
  ; target : Target.t (* stemming off of this *)
  }

let empty : t =
  { rev_stem = []
  ; all_inputs = Input_env.empty
  ; target = Target.empty
  }

let make target = { empty with target ; all_inputs = target.Target.i_env }

(** [cons p_item t] puts [p_item] as the most recent item in [t] only if the
  target has been passed. *)
let cons (p_item : Path_item.t) (t : t) : t =
  if Target.is_before t.target p_item.when_ then
    { t with rev_stem = (p_item, t.all_inputs) :: t.rev_stem }
  else
    t

(** [log kind key input t] logs the input only if target has been passed. *)
let log kind key v t =
  if Target.is_before t.target (Stepkey.step key) then
    { t with all_inputs = Input_env.add kind key v t.all_inputs }
  else
    t

let path_inputs t = t.all_inputs

let path_formulas (t : t) : bool Formula.t list =
  List.fold_left (fun acc (item, _) ->
    match item.Path_item.kind with
    | Formula { cond ; do_flip = _ } -> cond :: acc
    | Tag _ -> acc
  ) t.target.all_formulas t.rev_stem

let path_priority t =
  List.fold_left (fun acc (item, _) ->
    Priority.plus (Path_item.priority item) acc
  ) t.target.priority t.rev_stem

let forward_stem t =
  List.rev t.rev_stem
