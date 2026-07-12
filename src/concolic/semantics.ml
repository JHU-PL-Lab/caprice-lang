
open Lang
open Grammar

exception InvariantException of string

module State = struct
  type t =
    { stem : Stem.t (* we will cons to the path instead of union a log *)
    ; logged_inputs : Input_env.t
    ; runs : Logged_run.t list
    ; cells : Utils.Cell.Map.t
    }

  let empty : t =
    { stem = Stem.empty
    ; logged_inputs = Input_env.empty
    ; runs = []
    ; cells = Utils.Cell.Map.empty
    }

  (* Empty state loaded with some inputs already logged so they do not need to
    be logged again when read. *)
  let of_inputs inputs =
    { empty with logged_inputs = inputs }
end

module Context = struct
  type det_context =
    | Allowed
    | Disallowed

  type t =
    { target : Target.t
    ; det_context : det_context
    }
end

include Monad

type ('a, 'env) m = ('a, < err : Eval_result.t ; env : 'env ; state : State.t ; ctx : Context.t >) t

module Matches = Val.Make_match (struct
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
    match ctx.det_context with
    | Allowed -> accept () state step
    | Disallowed -> reject (Mismatch "Nondeterminism used when not allowed") state
  }

(*
  Do something with the current step count if that step count is past the
  target. This is used to avoid pushing to the path and logging inputs until the
  target has been reached.
*)
let[@inline] if_after_target (do_ : Step.t -> (unit, 'env) m) : (unit, 'env) m =
  let* step = step in
  let* { Context.target ; _ } = read_ctx in
  if Step.compare step (Target.step target) <= 0 then
    return ()
  else
    do_ step

(**
  [push_tag_to_path ?alternatives tag] pushes [tag] onto the path stem, and records
    the [alternatives] as the other inputs possible so that a target can be made
    from them.

  Because the state only tracks a stem instead of a full path, this only pushes
  to that stem if the step exceeds the target step.
*)
let push_tag_to_path ~(alternatives : Tag.t list) (tag : Tag.t) : (unit, 'env) m =
  if_after_target (fun step ->
    modify (fun (s : State.t) ->
      let kind = Path_item.Tag { tag ; alternatives } in
      let path_item =
        { Path_item.when_ = step ; logged_inputs = s.logged_inputs ; kind }
      in
      let stem = Stem.cons path_item s.stem in
      { s with stem }
    )
  )

(**
  [log_input kind a] logs the input [a] with kind [kind] to have been
    read at the current time.
*)
let[@inline] log_input (kind : 'a Input.Kind.t) (a : 'a) : (unit, 'env) m =
  let* step in
  modify (fun (s : State.t) ->
    { s with logged_inputs =
        Input_env.add kind (Stepkey step) a s.logged_inputs
    }
  )

(**
  [push_and_log_tag tag] pushes the [tag] to the path stem without alternatives
    and logs [tag] as the input. Both actions are with respect to the current
    time.
*)
let push_and_log_tag (tag : Tag.t) : (unit, 'env) m =
  if_after_target (fun _ ->
    (* Pushing the tag checks again if after target, but it is probably not that
      expensive to do this check twice. *)
    let* () = push_tag_to_path ~alternatives:[] tag in
    log_input KTag tag
  )

(**
  [push_formula_to_path ?allow_flip formula] pushes the formula to the path stem
    as a true formula, such that any evaluation following the same path again must
    satifisfy the formula. By default, a target will be made from the negation
    of the formula, unless [allow_flip] is false.
*)
let push_formula_to_path ?(allow_flip : bool = true)
  (formula : (bool, Stepkey.t) Smt.Formula.t) : (unit, 'env) m =
  if Smt.Formula.is_const formula then
    return ()
  else
    if_after_target (fun step ->
      (* This branch comes at a time after the target branch, so it needs to be
        put on the stem since the target for this does not know about it. *)
      modify (fun (s : State.t) ->
        let kind =
          if allow_flip then
            Path_item.Formula formula
          else
            Nonflipping formula
        in
        let path_item =
          { Path_item.when_ = step ; kind ; logged_inputs = s.logged_inputs }
        in
        let stem = Stem.cons path_item s.stem in
        { s with stem }
      )
    )

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
  let* { Context.target ; _ } = read_ctx in
  let* step = step in
  return (Input_env.find kind (Stepkey step) target.i_env)

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
  [target_to_here] is a target representing the path to the current program
    point. It is trivial to solve because its solution is the logged input
    environment. The step of the target is a dummy because everything after
    here is necessarily not known to the target, so anything pushed to the path
    (as long as steps are increasing) with this target will always get put on
    the stem; the target knows nothing.

  The implementation is hand-rolled because of the value restriction.

  Invariant: this should only be sequenced when the old target has been
    reached. It is asserted that this invariant holds.
*)
let target_to_here : 'env. (Target.t, 'env) m =
  { run = fun ~reject:_ ~accept state step _ { target ; _ } ->
    assert (Step.compare step (Target.step target) > 0);
    let priority =
      Priority.plus (Target.priority target) (Stem.priority state.stem)
    in
    let all_formulas = Stem.formulas state.stem @ target.all_formulas in
    accept (
      Target.make Formula.trivial all_formulas state.logged_inputs
        ~priority ~when_:Step.dummy
    ) state step
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
  let* { Context.det_context ; _ } = read_ctx in
  let* target = target_to_here in
  fork forked_m { target ; det_context }
    ~setup_state:(fun state -> { state with stem = Stem.empty })
    ~restore_state:
      (fun e ~og ~forked_state ->
        let forked_run =
          { Logged_run.stem = forked_state.stem
          ; target
          ; answer = Eval_result.to_answer e }
        in
        (* Note that the forked state runs include the original runs (see setup_state)
            so we will overwrite og runs; they are included inside forked_state.runs *)
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
  local_ctx (fun (ctx : Context.t) -> { ctx with det_context = Disallowed }) x

(**
  [allow_inputs x] runs [x] such that any [assert_inputs_allowed]
    is NOT a failure.
*)
let[@inline] allow_inputs (x : ('a, 'env) m) : ('a, 'env) m =
  local_ctx (fun (ctx : Context.t) -> { ctx with det_context = Allowed }) x

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
  [run x target] runs [x] with [target] as the context, beginning with
    empty state and environment.
*)
let run (x : ('a, Val.Env.t) m) (target : Target.t) : Eval_result.t * State.t =
  let state = State.of_inputs target.i_env in
  match run x state Env.empty { target ; det_context = Allowed } with
  | Ok _, state -> Done, state
  | Error e, state -> e, state
