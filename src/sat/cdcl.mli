(* Boolean SAT solver loop core *)
open Utils
open Formula
open Trail
open Solution

(** [next] is the message type that indicates the next step
    the main loop should propagate onto a solver state [t] *)
type next =
  | Decide
  | Conflict of literal list
  | Implication of literal list * literal

(** [bcp level trail formula] is the Boolean Constraint Propagation decision proc that searches
    for a satisfying truth table assignment for the given FORMULA STATE, bookkeeping LEVEL and TRAIL states
    per call

    Each call to bcp will either...
    - {b Implicate} a unit literal along with its reason clause
    - {b Decide} on a literal when there are no propagations left
    - Find a {b conflict clause} and return UNSAT if LEVEL is 0,
      or backtrack to some previous level < LEVEL and adds the
      learned conflict clauses to the FORMULA state
*)
val bcp : int -> trail -> formula -> solution

(** [backtrack_learn ~level clause trail formula] continues [bcp] by
    backjumping TRAIL to LEVEL and prepending the learned CLAUSE to FORMULA *)
val backtrack_learn : level:int -> clause -> trail -> formula -> solution

(** [decide ~lit level trail formula] continues [bcp] by deciding
    on LIT at [LEVEL + 1] and pushing that [Decided step] to TRAIL
    before forwarding FORMULA to the next iteration *)
val decide : lit:literal -> int -> trail -> formula -> solution

(** [cdcl formula] returns some list of literals that satisfies FORMULA if
    it exists. Otherwise, it returns [None] to indicate FORMULA is unsatisfiable *)
val cdcl : formula -> solution

val pp_next : uid:(Uid.t -> string) -> Format.formatter -> next -> unit
