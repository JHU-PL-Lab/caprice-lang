type 'k reduction =
  | Contradiction
  | Reduced of
      { residual : (bool, 'k) Formula.t
      ; extracted : 'k Model.t
      }

type 'k binding =
  | Binding : ('a, 'k) Symbol.t * 'a -> 'k binding

let binding_from_formula (formula : (bool, 'k) Formula.t) =
  match formula with
  | Formula.Binop (Equal, Key (I key), Const_int value)
  | Formula.Binop (Equal, Const_int value, Key (I key)) ->
    Some (Binding (I key, value))
  | Formula.Key (B key) ->
    Some (Binding (B key, true))
  | Formula.Not (Key (B key)) ->
    Some (Binding (B key, false))
  | _ -> None

let find_binding formula =
  match formula with
  | Formula.And clauses -> List.find_map binding_from_formula clauses
  | formula -> binding_from_formula formula

let reduce formula =
  let rec loop extracted residual =
    let residual =
      residual
      |> Ints.linearize
      |> Ints.tighten_bounds
    in
    match residual with
    | Formula.Const_bool false -> Contradiction
    | Formula.Const_bool true -> Reduced { residual ; extracted }
    | residual ->
      begin
        match find_binding residual with
        | None -> Reduced { residual ; extracted }
        | Some (Binding (symbol, value)) ->
          let residual = Formula.subst value symbol residual in
          let extracted =
            Model.merge (Model.singleton value symbol) extracted
          in
          loop extracted residual
      end
  in
  loop Model.empty formula
