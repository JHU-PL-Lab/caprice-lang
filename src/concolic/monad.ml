
open Grammar

(*
  Make a monad that has
    - CPS
    - State
    - Environment
    - Context (environment that changes less often)
    - Step count (stateful)
    - Error ("good" and "bad" continuations)

  The error continuation may be used simply to escape the
  typical continuation, but not always to convey some breaking
  failure case, so it is called with `escape`.

  All of the components (besides step count) are parametric so
  that they can be empty or universal, which conveys that a certain
  monadic value does not use them.  For example, a value that does
  not read from the environment will leave the environment parametric.

  Thus this is an indexed monad, where all indices are clumped into
  an object type.

  All values in this module are parametric in the indices.
*)
type ('a, 'x) t =
  { run : 'r.
      reject:('err -> 'state -> 'r) ->
      accept:('a -> 'state -> Step.t -> 'r) ->
      'state -> Step.t -> 'env -> 'ctx -> 'r
  } constraint 'x = < err : 'err ; env : 'env ; state : 'state ; ctx : 'ctx >
  [@@unboxed]
(* With flambda and compiler flag O3, it is faster to unbox. In all other
  combinations (of regular compiler, O3, flambda without O3), it is faster
  to leave this boxed. *)

let[@inline] bind (x : ('a, 'x) t) (f : 'a -> ('b, 'x) t) : ('b, 'x) t =
  { run = fun ~reject ~accept state step env ctx ->
      x.run state step env ctx ~reject ~accept:(fun x state step ->
          (f x).run ~reject ~accept state step env ctx
        )
  }

let ( let* ) = bind

(**
  Use [and>] to sequence definitions whose relative order has no meaning, yet
  their effects are nevertheless sequenced. This is used to document that
  certain bindings are independent and that no semantic ordering between the
  effects is intended.
  This is intentionally not an applicative combinator, and thus it is not named
  [and*]. Note the difference between the behavior of
    let* a = None and* b = Some (print_endline "hello world"; 0) in Some b;;
    hello world
    - : int option = None
  and
    let* a = None in let* b = Some (print_endline "hello world"; 0) in Some b;;
    - : int option = None
  in the option monad. This [and>] behaves like the second: it is identical
  to [... in let* ...] except that the second binding cannot use the first, and
  it is merely self-documenting that order does not matter, even though the
  values are still sequenced.
*)
let ( and> ) x y =
  { run = fun ~reject ~accept state step env ctx ->
      x.run state step env ctx ~reject ~accept:(fun a state step ->
        y.run state step env ctx ~reject ~accept:(fun b state step ->
          accept (a, b) state step
        )
      )
  }


let[@inline] return (a : 'a) : ('a, 'x) t =
  { run = fun ~reject:_ ~accept state step _ _ ->
      accept a state step
  }

(*
  -----------
  ENVIRONMENT
  -----------
*)

let read : ('env, < env : 'env ; .. >) t =
  { run = fun ~reject:_ ~accept state step env _ ->
      accept env state step
  }

let[@inline] local (f : 'env -> 'env) (x : ('a, < env : 'env ; .. > as 'x) t) : ('a, 'x) t =
  { run = fun ~reject ~accept state step env ctx ->
      x.run ~reject ~accept state step (f env) ctx
  }

let local' (env : 'e) (x : ('a, < env : 'e ; .. >) t) : ('a, < env : 'env ; .. >) t =
  { run = fun ~reject ~accept state step _ ctx ->
      x.run ~reject ~accept state step env ctx
  }

(*
  -------
  CONTEXT
  -------
*)

let read_ctx : ('ctx, < ctx : 'ctx ; .. >) t =
  { run = fun ~reject:_ ~accept state step _ ctx ->
      accept ctx state step
  }

let[@inline] local_ctx (f : 'ctx -> 'ctx) (x : ('a, < ctx : 'ctx ; .. >) t)
  : ('a, < ctx : 'ctx ; .. >) t =
  { run = fun ~reject ~accept state step env ctx ->
      x.run ~reject ~accept state step env (f ctx)
  }

let[@inline] local_ctx' (ctx: 'ctx) (x : ('a, < ctx : 'ctx ; .. >) t)
  : ('a, < ctx : 'ctx ; .. >) t =
  { run = fun ~reject ~accept state step env _ ->
      x.run ~reject ~accept state step env ctx
  }

(*
  -----
  STATE
  -----
*)

let get : ('state, < state : 'state ; .. >) t =
  { run = fun ~reject:_ ~accept state step _ _ ->
      accept state state step
  }

let[@inline] modify (f : 'state -> 'state) : (unit, < state : 'state ; .. >) t =
  { run = fun ~reject:_ ~accept state step _ _ ->
      accept () (f state) step
  }

(*
  -----
  ERROR
  -----
*)

let[@inline] escape (err : 'err) : ('a, < err : 'err ; .. >) t =
  { run = fun ~reject ~accept:_ state _ _ _ ->
      reject err state
  }

(*
  ------------------
  ESCAPING THE MONAD
  ------------------
*)

let run (x : ('a, < err : 'err ; env : 'env ; state : 'state ; ctx : 'ctx >) t)
  (init_state : 'state) (init_env : 'env) (init_ctx : 'ctx)
  : ('a * Step.t, 'err) result * 'state =
  x.run init_state Step.zero init_env init_ctx
    ~reject:(fun e state -> Error e, state)
    ~accept:(fun a state step -> Ok (a, step), state)

(*
  -----------------
  INTERPRETER STUFF
  -----------------
*)

let step : (Step.t, 'x) t =
  { run = fun ~reject:_ ~accept state step _ _ ->
      accept step state step
  }

let[@inline] fork (m : 'a. ('a, < err : 'err ; state : 'state ; .. > as 'x) t)
  (k : 'err -> ('a, 'x) t) ~(setup_state : 'state -> 'state)
  ~(restore_state : 'err -> og:'state -> forked_state:'state -> 'state)
  : ('a, 'x) t =
  { run = fun ~reject ~accept state step env ctx ->
    m.run (setup_state state) step env ctx
      ~accept:Utils.Empty.absurd
      ~reject:(fun e forked_state ->
        (* uses original step count when resuming, not step count after fork *)
        (k e).run ~reject ~accept (restore_state e ~og:state ~forked_state) step env ctx
      )
  }
