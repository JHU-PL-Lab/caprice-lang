open Formula

type model = Formula.literal list

let find_opt atom model =
  List.find_opt (fun lit -> atom_from_literal lit = atom) model

let find atom model =
  match find_opt atom model with
  | None -> failwith
    (Printf.sprintf "\n[Sat.Model.find]: atom with uid %d doesn't exist"
      (Utils.Uid.to_int @@ atom))
  | Some lit -> lit

let (#::) (x : 'a) (xs : ('a list) option) : ('a list) option =
  match xs with
  | None -> None
  | Some xs' -> Some (x :: xs')

let rec eval_clause clause model =
  match clause with
  | [] -> `Falsified
  | lit :: clause' ->
    match find_opt (atom_from_literal lit) model with
    | None ->
      begin match eval_clause clause' model with
      | `Satisfied -> `Satisfied
      | `Falsified -> `Undecided [lit]
      | `Undecided lits -> `Undecided (lit :: lits)
      end
    | Some lit' ->
      if lit = lit' then `Satisfied
      else eval_clause clause' model

(** [is_tautology formula model] returns true if subbing in
    the literals from MODEL results in the empty formula. *)
let is_tautology formula model =
  List.for_all
    (fun clause ->
      match eval_clause clause model with
      | `Satisfied -> true
      | `Falsified
      | `Undecided _ -> false)
    formula

let pp_model ~uid fmt (m : literal list) =
  List.iter (Format.fprintf fmt "%a " (pp_literal ~uid)) m
