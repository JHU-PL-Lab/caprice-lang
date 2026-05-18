open Formula

type next =
  | Decide
  | Conflict of literal list
  | Implication of literal list * literal

let pp_next ~uid fd (next : next) : unit =
  match next with
  | Decide ->
      Format.fprintf fd "Decide"
  | Conflict clause ->
      Format.fprintf fd "Conflict (%a)" (pp_clause ~uid) clause
  | Implication (clause, lit) ->
      Format.fprintf fd "Implication (%a, %a)" (pp_clause ~uid) clause (pp_literal ~uid) lit

(** [unit_propagate formula trail] iterates over the clauses from FORMULA and for each clause,
    subs in the literal values from TRAIL, and returns the [next] step based on
    the resulting substituted clause. The step will be...
    - [Decide] when there are no unit clauses after applying TRAIL to FORM
    - [Implication (clause, lit)]
    - [Conflict clause] when applying TRAIL to clause is inconsistent (i.e. substitution returns [Some] empty list)
*)
let unit_propagate formula model =
  let rec search_empty
    (formula : Formula.formula)
    (reason_clause : literal list)
    (lit : Formula.literal)
    : next =
    match formula with
    | [] -> Implication (reason_clause, lit)
    | clause :: clauses' ->
      match Model.eval_clause clause model with
      | `Falsified -> Conflict clause
      | _ -> search_empty clauses' reason_clause lit
  in
  let rec search_unit (formula : Formula.formula) : next =
    match formula with
    | [] -> Decide
    | clause :: clauses' ->
      match Model.eval_clause clause model with
      | `Falsified -> Conflict clause
      | `Undecided [lit] -> search_empty clauses' clause lit
      | _ -> search_unit clauses'
  in
  search_unit formula

let rec bcp (level : int) (trail : Trail.trail) (formula : Formula.formula) : Solution.solution =
  let model = Trail.to_model trail in
  begin match unit_propagate formula model with
  | Decide ->
    let atoms = List.map Formula.atom_from_literal model in
    begin match Formula.find_free_variable_opt atoms formula with
    | None ->
      if Model.is_tautology formula model then SAT model
      else UNSAT
    | Some x -> decide ~lit:(Formula.pos x) (level + 1) trail formula
      (* [Formula.pos x] is arbitrary. It doesn't matter because the
          learned conflicts forces the loop to terminate (at some point).

          Smarter decision heuristics could be implemented in the future... *)
    end
  | Conflict clause ->
    let clause', backtrack_lvl = Trail.analyze_conflict ~clause level trail in
    if backtrack_lvl < 0 then UNSAT
    else backtrack_learn ~level:backtrack_lvl clause' trail formula
  | Implication (clause, lit) ->
    let trail' = Trail.imply ~reason:clause level lit trail in
    bcp level trail' formula
  end

and backtrack_learn ~level clause trail formula =
  let trail' = Trail.backjump ~level trail in
  let formula' = clause :: formula in
  bcp level trail' formula'

and decide ~lit next_lvl trail =
  let trail' = Trail.decide ~lit next_lvl trail in
  bcp next_lvl trail'

let cdcl formula = bcp 0 [] formula
