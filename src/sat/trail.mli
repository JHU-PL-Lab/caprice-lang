(** The reason type encodes {i why} an assignment by the CDCL loop was made

    The CDCL loop advances via {b propagatation} when there are unit clauses
    implied after propagation to force truth values for.

    The list held by [Propagated] reasons can be a singleton list in the
    unit clause case. For example [[a ^ b ^ (c v d) ^ (~c v f)]]
    will have it so that [a] is added to the trail as [Propagated [a]]
    and likewise for [b].

    Once there are no more unit propagations to be made, the loop needs to
    {b decide} on a truth value for a disjunctive prop, in which case
    the [Decided] reason should be added to the trail.

    Continuing with our working example, after we unit propagate, we are left with:

    [[(c v d) ^ (~c v f)]]

    Suppose CDCL solver decides [c] as the asserted literal. Then the [Decided]
    step entry will be added to the trail, and any implied propagations will
    link back to the decision via its propagated list.

    After deciding on [c], our formula has intermediate state:

    [[ (true v d) ^ (false v f) ]]

    Which simplifies to

    [[ f ]]

    Then when we propagate [f = true], its Propagate list will be:

    [[~c ; f]]

    because f was forced to be [true] because [c = true].
*)
type reason = private
  | Decided
  | Propagated of Formula.clause

(** A step of [{ level ; lit ; reason }] means the solver assigned LIT for REASON at decision LEVEL *)
type step = { level : int ; lit : Formula.literal ; reason : reason }

(** A trail is the list of steps that acts as the source of truth for the solver state *)
type trail = step list

(** [to_model trail] derives the boolean MODEL from TRAIL *)
val to_model : trail -> Model.model

(** [analyze_conflict ~clause level trail] returns the resolved learned clause
    derived from CLAUSE and the next highest decision level after LEVEL
    from a literal pointed to by both CLAUSE and TRAIL *)
val analyze_conflict : clause:Formula.clause -> int -> trail -> Formula.clause * int

(** [backjump ~level trail] {i backtracks} the model state by removing
    all steps in TRAIL with a [level] > LEVEL *)
val backjump : level:int -> trail -> trail

(** [decide ~lit level trail] prepends step with [reason = Decided]
    after solver loop has {i decided} on LIT at LEVEL *)
val decide : lit:Formula.literal -> int -> trail -> trail

(** [imply ~reason level lit trail] {i implies} LIT at decision LEVEL
    by prepending step with [reason = Propagated] to TRAIL *)
val imply : reason:Formula.clause -> int -> Formula.literal -> trail -> trail
