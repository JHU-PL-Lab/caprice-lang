type 'k atom =
(** A ['k atom] captures all structs that are boolean in value for a formula with 'k keys *)
  | Bool_key of (bool, 'k) Symbol.t
  | Predicate : ('a * 'a * bool) Binop.t * ('a, 'k) Formula.t * ('a, 'k) Formula.t -> 'k atom

type 'k literal =
  | Pos of 'k atom
  | Neg of 'k atom

type 'k clause = Clause of 'k literal list

let clause lits = Clause lits

type 'k core = Core of 'k literal list

type 'k formula = 'k clause list

type 'k theory_solution =
  | Theory_unknown
  | Theory_sat of 'k Model.t
  | Theory_unsat of 'k core
  | Theory_split of 'k formula

let sat (model : 'k Model.t)
  : 'k theory_solution = Theory_sat model

let unsat (core : 'k literal list)
  : 'k theory_solution = Theory_unsat (Core core)

let unknown = Theory_unknown

let split (formula : 'k formula) = Theory_split formula

type 'k theory_solver = 'k literal list -> 'k theory_solution
(** A ['k theory_solver] accepts a list implied to be in a conjunction and returns its domain specific ['k t_solution] *)

let from_smt_atom (atom : (bool, 'k) Formula.t) : 'k atom =
  match atom with
  | Formula.Key key -> Bool_key key
  | Formula.Binop (op, left, right) -> Predicate (op, left, right)
  | Formula.Not _ | Formula.And _ | Formula.Const_bool _ ->
    failwith
      (Printf.sprintf "[Theory.from_smt_atom] that's not a Key or a Binop: %s"
        (Formula.to_string ~key:Model.prefix_key atom))

let from_smt_literal (lit : (bool, 'k) Formula.t) : 'k literal =
  match lit with
  | Formula.Not inner -> Neg (from_smt_atom inner)
  | inner -> Pos (from_smt_atom inner)

let from_smt_clause (clause : (bool, 'k) Formula.t list) : 'k clause =
  Clause (List.map from_smt_literal clause)

let from_smt_formula (form : (bool, 'k) Formula.t) : 'k formula =
  form
  |> Formula.clauses_from
  |> List.map Formula.disjuncts_from_clause
  |> List.map from_smt_clause

let pp_atom ~key fmt (atom : 'k atom) : unit =
  match atom with
  | Bool_key (B id) ->
      Format.fprintf fmt "B%d" (Utils.Uid.to_int id)

  | Predicate (binop, left, right) ->
      Format.fprintf fmt "(%s %s %s)"
        (Formula.to_string left ~key)
        (Binop.to_string binop)
        (Formula.to_string right ~key)

let pp_literal ~key fmt (lit : 'k literal) : unit =
  match lit with
  | Pos atom ->
      Format.fprintf fmt "%a" (pp_atom ~key) atom
  | Neg atom ->
      Format.fprintf fmt "(not %a)" (pp_atom ~key) atom

let pp_clause ~key fmt (Clause clause: 'k clause) : unit =
  match clause with
  | [Pos _ as lit]
  | [Neg _ as lit] ->
      Format.fprintf fmt "@[%a@]" (pp_literal ~key) lit
  | _ ->
      Format.fprintf fmt "(@[%a@])"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " v@ ")
           (pp_literal ~key))
        clause

let pp_delim_literals ?(delim = "\n") ~key fmt (lits : 'k literal list) : unit =
  Format.fprintf fmt "@[%a@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "%s" delim)
       (pp_literal ~key))
    lits

let pp_unit_literals ~key fmt (lits : 'k literal list) : unit =
  Format.fprintf fmt "@[%a@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt " ^@ ")
       (pp_literal ~key))
    lits

let pp_formula ~key fmt (formula : 'k formula) : unit =
  Format.fprintf fmt "@[%a@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt " ^@ ")
       (pp_clause ~key))
    formula

let pp_theory_solution
  (type k)
  ~(key : Model.key -> string)
  (fmt : Format.formatter)
  (solution : k theory_solution)
  : unit =
  match solution with
  | Theory_unknown -> Format.fprintf fmt "(T) UNKNOWN"
  | Theory_sat model ->
    Format.fprintf fmt "(T) SAT %s" (Model.to_string ~key model)
  | Theory_unsat Core core_lits ->
    Format.fprintf fmt "(T) UNSAT %a" (pp_unit_literals ~key) core_lits
  | Theory_split split -> Format.fprintf fmt "(T) SPLIT %a" (pp_formula ~key) split
