(* Blue3 is the 'glue' layer between the CDCL SAT solver and
   the Theory layer. It is responsible for mapping the propositional SAT formula atoms with those from the SMT formula. *)

open Utils

type 'k connector

val make : int -> 'k connector

val abstract : ?uid:(int -> Uid.t) -> 'k Theory.formula -> 'k connector -> Sat.Formula.formula

val abstract_clause : ?uid:(int -> Uid.t) -> 'k Theory.clause -> 'k connector -> Sat.Formula.clause

val make_theory_literals : Sat.Model.model -> 'k connector -> 'k Theory.literal list

val theory_learn : 'k Theory.core -> 'k connector -> Sat.Formula.clause

(** [cdcl_T ~theory_solver formula] runs the non-incremental CDCL (T) loop
    using a single THEORY_SOLVER against FORMULA, or the so-called
    "simple" CDCL (T) loop from the
    {{: https://theory.stanford.edu/~nikolaj/programmingz3.html#sec-cdclt } Programming Z3} docs.

    It uses the {{: https://people.eecs.berkeley.edu/~sseshia/pubdir/SMT-BookChapter.pdf } case splitting on demand heuristic}
    to append the disjunction cases that the CDCL boolean loop should decide on.

    One of the reasons it is "simple" because it waits for [Sat.Cdcl.cdcl]
    to spit out a *full* satisfying boolean assignment before checking
    for (T) Satisfiability.

    A more optimized solver would check for (T) Satisfiability at each decision
    made by the core CDCL boolean solver so it can cut off bad branches earlier,
    but at the time of writing, that functionality doesn't seem to be necessary.

    Maybe by somebody else in the future... *)
val cdcl_T : solver:'k Theory.theory_solver -> (bool, 'k) Formula.t -> 'k Solution.t

(** [blue3 ~solver next formula] attempts to find a Satifisable model
    for FORMULA that is consistent with theory SOLVER. If SOLVER returns [Theory_unknown],
    then this immediately calls NEXT *)
val blue3 :
  solver:'k Theory.theory_solver ->
    ((bool, 'k) Formula.t -> 'k Solution.t) -> (bool, 'k) Formula.t -> 'k Solution.t
