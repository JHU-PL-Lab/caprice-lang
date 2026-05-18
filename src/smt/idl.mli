(** [solve_int_diff lits] finds the tightest upper bounds of each integer
    variable in LITS and packs them into a [Model.t], if they exist

    If there are any [Not (Binop (Equal, _, _)] literals, then this returns
    the implied literals from those neqs as the [Theory_split].

    Otherwise, a contradiction was found, so this will return the
    UNSAT core literals that made up the contradiction so that the CDCL
    can advance. In the case of IDL, a contradiction means a negative cycle
    was detected in the underlying graph search proc.
*)
val solve_int_diff : 'k Theory.literal list -> 'k Theory.theory_solution
