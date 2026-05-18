(* Operations to work with a pure boolean model. In this implementation,
   it is represented as a plain list of [Formula.literal]s because
   that is sufficient to encode the assignments. *)

(** Encodes a truth-table-like model that asserts its contained literals as the assignments *)
type model = Formula.literal list

(** [find_opt atom model] returns the literal corresponding to ATOM from MODEL if it exists *)
val find_opt : Formula.atom -> model -> Formula.literal option

(** [find atom model] returns the literal corresponding to ATOM from MODEL if it exists, otherwise it throws *)
val find : Formula.atom -> model -> Formula.literal

(** [eval_clause clause model] evaluates the literals in CLAUSE with respect
    to its atom assignments from MODEL

    If CLAUSE is empty, then this immediately returns [`Falsified] as this means
    we've exhausted all possible literals without finding a single match.

    Otherwise, we iterate over each [literal] in CLAUSE. If we can find
    at least one literal from MODEL that matches its value in CLAUSE,
    then we immediately return [`Satisfied] (since 1 [true] results in the
    whole clause becoming [true]).

    If the [literal] from CLAUSE and MODEL don't agree (we get [p ^ ~p])
    OR the [literal] value couldn't be found in MODEL, then we drop
    [literal] from the working literal list and return the result of running
    [eval_clause] recursively on the tail.

    The recursive return value depends on which of the 2 cases it was called on:

    1. If [literal] exists in MODEL, but it is the opposite value (not equal)
       of that from CLAUSE, then the return value is just whatever the recursive
       call on the tail returns.
    2. If [literal] {i does not} exist in MODEL, then it will return either
       [`Satisfied] if the recursive case finds that. If it gets `Falsified,
       then we return [`Undecided [literal]], which begins the tail of
       the [`Undecided] literal list from which case the subsequent matches
       against [`Undecided] will append its literal to the head to.
*)
val eval_clause : Formula.clause -> model ->
  [ `Falsified
  | `Satisfied
  | `Undecided of Formula.clause
  ]

(** [is_tautology formula model] returns true if substituting the literals from
  MODEL satisfies every clause in FORMULA, reducing it to the empty formula *)
val is_tautology : Formula.formula -> model -> bool

val pp_model : uid:(Utils.Uid.t -> string) -> Format.formatter -> model -> unit
