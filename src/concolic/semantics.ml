
open Lang
open Grammar

exception InvariantException of string

module State = struct
  type t =
    { stem : Stem.t
    ; runs : Logged_run.t list
    ; cells : Utils.Cell.Map.t
    }

  let make goal =
    { stem = Stem.make goal
    ; runs = []
    ; cells = Utils.Cell.Map.empty
    }
end

(* Context: whether nondeterminism is allowed or not *)
type det_ctx = Allow_inputs | Disallow_inputs

include Monad

type ('a, 'env) m = ('a, < err : Eval_result.t ; env : 'env ; state : State.t ; ctx : det_ctx >) t

module Matching = Val.Match.Make (struct
  type nonrec 'a m = ('a, Val.Env.t) m
  include (Monad : Utils.Types.MONAD with type 'a m := 'a m)
end)

let[@inline] incr_step
  : 'env. max_step:Step.t -> (unit, 'env) m
  = fun ~max_step ->
  { run = fun ~reject ~accept state step _ _ ->
      let step = Step.next step in
      if Step.(step > max_step)
      then reject (Eval_result.Reach_max_step step) state
      else accept () state step
  }

(**
  [fetch id] is the value associated with [id] in the environment,
    or failure if [id] is unbound.
*)
let[@inline] fetch (id : Ident.t) : (Val.any, Val.Env.t) m =
  { run = fun ~reject ~accept state step env _ ->
      match Env.find id env with
      | None -> reject (Eval_result.Unbound_variable id) state
      | Some v -> accept v state step
  }

(* For typing purposes (due to value restriction), we must inline the
  definition of `Monad.escape`.

  The ideal implementation would simply be `escape Vanish`.
*)
let vanish : 'a 'env. ('a, 'env) m =
  { run = fun ~reject ~accept:_ state _ _ _ -> reject Vanish state }

let mismatch : 'a 'env. string -> ('a, 'env) m = fun msg ->
  escape (Eval_result.Mismatch msg)

(**
  [assert_inputs_allowed] is a failure if the context disallows inputs.
*)
let assert_inputs_allowed : 'env. (unit, 'env) m =
  { run = fun ~reject ~accept state step _ ctx ->
    match ctx with
    | Allow_inputs -> accept () state step
    | Disallow_inputs -> reject (Mismatch "Nondeterminism used when not allowed") state
  }

let modify_stem f =
  let* step in
  modify (fun (s : State.t) ->
    let stem = f step s.stem in
    if stem == s.stem then s else { s with stem }
  )

let cons_to_stem kind =
  modify_stem (fun step stem ->
    let item = { Path_item.when_ = step ; kind } in
    Stem.cons item stem
  )

(**
  [push_tag_to_path ?alternatives tag] pushes [tag] onto the path stem, and
  records the [alternatives] as the other inputs possible so that a target can
  be made from them.
*)
let push_tag_to_path ~(alternatives : Tag.t list) (tag : Tag.t) : (unit, 'env) m =
  cons_to_stem (Path_item.Tag { tag ; alternatives })

(**
  [log_input kind a] logs the input [a] with kind [kind] to have been read at
  the current time.
*)
let[@inline] log_input (kind : 'a Input.Kind.t) (a : 'a) : (unit, 'env) m =
  modify_stem (fun step stem -> Stem.log kind (Stepkey step) a stem)

(**
  [push_and_log_tag tag] pushes the [tag] to the path stem without alternatives
    and logs [tag] as the input. Both actions are with respect to the current
    time.
*)
let push_and_log_tag (tag : Tag.t) : (unit, 'env) m =
  (* Both pushing the tag and logging the input check (internally, inside the
    stem) that the goal has been passed, but the check is relatively cheap. *)
  let* () = push_tag_to_path ~alternatives:[] tag in
  log_input KTag tag

(**
  [push_formula_to_path ?allow_flip ~max_step formula] pushes the formula to the
  path stem as a true formula, such that any evaluation following the same path
  again must satifisfy the formula. By default, a target will be made from the
  negation of the formula, unless [allow_flip] is false.

  Increments the step beforehand to ensure this target comes at a unique time,
  so the max step is a parameter.
*)
let push_formula_to_path ?(allow_flip : bool = true) ~(max_step : Step.t)
  (formula : (bool, Stepkey.t) Smt.Formula.t) : (unit, 'env) m =
  let* () = incr_step ~max_step in
  if Smt.Formula.is_const formula then
    return ()
  else
    cons_to_stem (Path_item.Formula { cond = formula ; do_flip = allow_flip })

(**
  [read_input kind input_env] is an optional input from [input_env] with the
    kind [kind], read from the current time. Does not log the input as read
    because the default behavior is to return [None], in which case there
    is no input to log.

  If the input is a tag, then the caller is responsible for calling
  [push_and_log_tag] afterwards without incrementing the step beforehand.
  This is often done within forking.
*)
let read_input (kind : 'a Input.Kind.t) : ('a option, 'env) m =
  let* () = assert_inputs_allowed in
  let* step in
  let* { State.stem ; _ } = get in
  return (Input_env.find kind (Stepkey step) stem.goal.assignments)

(**
  [read_and_log_input kind input_env ~default] is an input from [input_env]
    of the kind [kind], or [default] if the input was unplanned. Then, the
    input is logged as read from the environment, and it is returned.

  VERY IMPORTANT:
    If the input is a tag, then the caller is responsible for pushing that tag
    to the path (with whatever alternatives) without incrementing the step
    beforehand. This is not done here because the alternatives are not known.
    Do this by calling [push_tag_to_path] immediately afterwards.
*)
let read_and_log_input (kind : 'a Input.Kind.t) ~(default : 'a) : ('a, 'env) m =
  let* input_opt = read_input kind in
  match input_opt with
  | Some input ->
    return input
  | None ->
    let* () = log_input kind default in
    return default

(**
  [reset_goal] makes the goal in the state this exact point in the current
  program execution. The step of the goal is a dummy because everything after
  here is necessarily not known to the goal. Therefore, anything pushed to the
  path with this goal will always be put on the stem.

  Logs all the work done already as a run so that future work can stem off of
  this new goal.

  The implementation is hand-rolled because of the value restriction.

  Invariant: this should only be sequenced when the old goal has been reached.
  It is asserted that this invariant holds.
*)
let reset_goal : 'env. (unit, 'env) m =
  { run = fun ~reject:_ ~accept state step _ _ ->
    let stem = state.stem in
    assert (Goal.is_before stem.goal step);
    accept ()
      { state with
        stem = Stem.make (Stem.contract stem)
      ; runs = { stem ; answer = Exhausted } :: state.runs
      } step
  }

(**
  [fork forked_m] runs [forked_m] with the current state, environment, and
    step count. If [forked_m] is a failure case, then the result is a failure.
    Otherwise, the original state is restored, and the fork is logged as a
    run.
    Calls [Utils.Time.yield_to_timer] because this is a good moment to check for
    time out. Therefore, this function must be run inside [Utils.Time.with_timeout]
    so that the effect is handled.
*)
let fork (forked_m : 'a. ('a, 'env) m) : (unit, 'env) m =
  let* () = reset_goal in
  fork forked_m
    ~setup_state:Fun.id
    ~restore_state:
      (fun e ~og ~forked_state ->
        (* Note that the forked state runs include the original runs (see setup_state)
            so we will overwrite og runs; they are included inside forked_state.runs *)
        let forked_run =
          { Logged_run.stem = forked_state.stem ; answer = Eval_result.to_answer e }
        in
        { og with runs = forked_run :: forked_state.runs }
      )
    (fun res ->
      if Eval_result.is_signal_to_stop res then
        escape res (* propagate up the failure *)
      else begin
        Utils.Time.yield_to_timer (); return ()
      end
    )

let get_cell
  : type a env. a Utils.Cell.t -> (a, env) m
  = fun key ->
  let* (s : State.t) = get in
  return (Utils.Cell.Map.find key s.cells)

let set_cell
  : type a env. a Utils.Cell.t -> a -> (unit, env) m
  = fun key v ->
  modify (fun (s : State.t) ->
    { s with cells = Utils.Cell.Map.add key v s.cells }
  )

let new_cell
  : type a env. a -> (a Utils.Cell.t, env) m
  = fun a ->
  let key = Utils.Cell.new_cell () in
  let* () = set_cell key a in
  return key

let new_lazy_cell : 'env. Val.lgen -> (Val.dval, 'env) m = fun lgen ->
  let* cell = new_cell (Val.LLazy lgen) in
  return (Val.VLazy { cell ; wrapping_types = [] })

(**
  [disallow_inputs x] runs [x] such that any [assert_inputs_allowed]
    is a failure.
*)
let[@inline] disallow_inputs (x : ('a, 'env) m) : ('a, 'env) m =
  local_ctx' Disallow_inputs x

(**
  [allow_inputs x] runs [x] such that any [assert_inputs_allowed]
    is NOT a failure.
*)
let[@inline] allow_inputs (x : ('a, 'env) m) : ('a, 'env) m =
  local_ctx' Allow_inputs x

(**
  [local_mode mode x] runs [x] in the context based on
    the [mode] of the function type that is being checked.

    The context disallows inputs if the mode is deterministic.
*)
let local_mode (mode : Funtype.mode) (x : ('a, 'env) m) : ('a, 'env) m =
  match mode with
  | Nondet -> x
  | Det -> disallow_inputs x

(**
  [run x goal] runs [x] towards the [goal], beginning with empty state and
  environment. Uses the input environment from [goal].
*)
let run (x : ('a, Val.Env.t) m) (goal : Goal.t) : Eval_result.t * State.t =
  let state = State.make goal in
  match run x state Env.empty Allow_inputs with
  | Ok _, state -> Done, state
  | Error e, state -> e, state
