
type t =
  { target_formula : bool Formula.t
  ; all_formulas : bool Formula.t list
  ; i_env : Input_env.t
  ; id : Utils.Uid.t
  ; when_ : Step.t
  ; priority : Priority.t }

let empty : t =
  { target_formula = Formula.trivial
  ; all_formulas = []
  ; i_env = Input_env.empty
  ; id = Utils.Uid.make_new ()
  ; when_ = Step.dummy
  ; priority = Priority.zero }

(**
  [make last_formula other_formula i_env ~priority ~when_] makes a target whose
    respective program path has priority [priority]. The target can be run if
    [last_formula] and all formulas in [other_formulas] are satisfied. [i_env]
    is an input environment that satisfies [other_formulas] in isolation. The
    target will been reached at [when_] steps of evaluation.

    Thus, if [i_env] is extended with the solution to connected component of
    [last_formula] with respect to [other_formulas], then we have an input
    environment that satisfies the made target.

    This "connected component" optimization originates with EXE. See the comment
    in Formula.BSet.scc for an explanation.

    The made target gets a brand new unique identifier (with respect to any
    other target made in this way), which is the sole basis of equality and
    comparison. Therefore, no two targets that represent the same program path
    should be made, or else they will be unequal.
*)
let make (last_formula : bool Formula.t) (other_formulas : bool Formula.t list)
  (i_env : Input_env.t) ~(priority : Priority.t) ~(when_ : Step.t) : t =
  let target_formula = Smt.Formula.scc last_formula ~wrt:other_formulas in
  let all_formulas = last_formula :: other_formulas in
  let id = Utils.Uid.make_new () in
  { target_formula ; all_formulas ; i_env ; id ; when_ ; priority }

let compare a b =
  Utils.Uid.compare a.id b.id

let equal a b =
  Utils.Uid.equal a.id b.id

let priority ({ priority ; _ } : t) : Priority.t =
  priority

let step ({ when_ ; _ } : t) : Step.t =
  when_
