(* Handles working with Boolean level propositional formulas *)

(** A SAT level atom is just a propositional variable, represented
    via some unique id [Uid.t] *)
type atom = Utils.Uid.t

(** A literal is either the positive assertion of an [atom],
    or the assertion that [atom] is negated *)
type literal = private
  | Pos of atom
  | Neg of atom

(** A clause is a list of [literal]s meant to be interpreted as disjunctive *)
type clause = literal list

(** A 2d list of literals encodes a CNF formula, where the list elements
    should be interpreted to be in a conjunction. Each list element is
    a clause, and inside each clause are literals that should
    be interpreted to be in a disjunction. *)
type formula = literal list list

(** [pos atom] is the positive asserted ATOM literal *)
val pos : atom -> literal

(** [neg atom] is the negated asserted ATOM literal *)
val neg : atom -> literal

(** [negate lit] is the opposite literal of LIT *)
val negate : literal -> literal

(** [atom_from_literal lit] is the atom unwrapped from LIT *)
val atom_from_literal : literal -> atom

(** [find_free_variable exclude formula] finds the first atom left-to-right
    from FORMULA's clauses that is not a member of EXCLUDE *)
val find_free_variable_opt : atom list -> formula -> atom option

(** [resolve_pair c1 c2] concatenates C1 with C2 and drops any duplicate literals between the two clauses *)
val resolve_pair : clause -> clause -> clause

(** [conjoin1 clause formula] constructs a new formula by prepending CLAUSE to FORMULA's head *)
val conjoin1 : literal list -> formula -> formula

val pp_literal : uid:(Utils.Uid.t -> string) -> Format.formatter -> literal -> unit
val pp_clause : uid:(Utils.Uid.t -> string) -> Format.formatter -> literal list -> unit
val pp_formula : uid:(Utils.Uid.t -> string) -> Format.formatter -> formula -> unit
