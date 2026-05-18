open Utils

type atom = Uid.t
type literal =
  | Pos of atom
  | Neg of atom
type clause = literal list
type formula = literal list list

let pos (atom : atom) = Pos atom
let neg (atom : atom) = Neg atom

let negate (lit : literal) : literal =
  match lit with
  | Pos n -> Neg n
  | Neg n -> Pos n

let atom_from_literal (lit : literal) : atom =
  match lit with
  | Pos n
  | Neg n -> n

let find_free_variable_opt exclude formula : atom option =
  formula
  |> List.flatten
  |> List.find_opt (fun lit -> not (List.mem (atom_from_literal lit) exclude))
  |> Option.map atom_from_literal

let disjoin (clause1 : literal list) (clause2 : literal list) : literal list =
  List.fold_right
    (fun lit clause2 ->
      if (List.mem lit clause2) then clause2
      else lit :: clause2)
    clause1 clause2

let conjoin1 (clause : literal list) (formula : formula) : formula = clause :: formula

let resolve_pair clause1 clause2 =
  let l1, l2 = List_utils.find_pair
    (fun lit1 lit2 -> lit1 = negate lit2)
    clause1 clause2
  in
  disjoin (List_utils.remove1 l1 clause1) (List_utils.remove1 l2 clause2)

let pp_literal ~(uid : Uid.t -> string) fmt (lit : literal) : unit =
  let prefix =
    match lit with
    | Pos _ -> ""
    | Neg _ -> "~"
  in
  Format.fprintf fmt "%s%s" prefix (uid (atom_from_literal lit))

let pp_clause ~(uid : Uid.t -> string) fmt (clause : literal list) : unit =
  match clause with
  | [lit] ->
      Format.fprintf fmt "@[%a@]" (pp_literal ~uid) lit
  | _ ->
      Format.fprintf fmt "(@[%a@])"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " v@ ")
           (pp_literal ~uid))
        clause

let pp_formula ~(uid : Uid.t -> string) fmt (form : formula) : unit =
  if form = [] then
    Format.fprintf fmt "true"
  else
    Format.fprintf fmt "@[%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " ^@ ")
         (pp_clause ~uid))
      form
