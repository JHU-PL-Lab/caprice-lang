
(* Raised when any bound is unsatisfiable. *)
exception Unsat

module IntSet = Set.Make (Int)

module Bounds = struct
  type t =
    { lower : int option
    ; upper : int option
    ; excluded : IntSet.t
    }

  let unbounded =
    { lower = None
    ; upper = None
    ; excluded = IntSet.empty
    }

  let find_bounds key bounds_by_key =
    match Utils.Uid.Map.find_opt key bounds_by_key with
    | Some bounds -> bounds
    | None -> unbounded

  let with_lower candidate bounds =
    let lower =
      match bounds.lower with
      | None -> candidate
      | Some current -> max current candidate
    in
    { bounds with lower = Some lower }

  let with_upper candidate bounds =
    let upper =
      match bounds.upper with
      | None -> candidate
      | Some current -> min current candidate
    in
    { bounds with upper = Some upper }

  let set_equal value bounds =
    bounds
    |> with_lower value
    |> with_upper value

  let exclude value bounds =
    { bounds with excluded = IntSet.add value bounds.excluded }

  (* Quick path to check contradictions. Is not complete. Raises Unsat. *)
  let check_clearly_contradictory bounds =
    match bounds.lower, bounds.upper with
    | Some lower, Some upper ->
      if
        lower > upper
        || (lower = upper && IntSet.mem lower bounds.excluded)
      then
        raise_notrace Unsat
    | _ -> ()

  let rec drop_low_exclusions bounds =
    match bounds.lower with
    | Some lower when IntSet.mem lower bounds.excluded ->
      if lower = Int.max_int then raise Solution.Overflow;
      drop_low_exclusions { bounds with lower = Some (lower + 1) }
    | _ -> bounds

  let rec drop_high_exclusions bounds =
    match bounds.upper with
    | Some upper when IntSet.mem upper bounds.excluded ->
      if upper = Int.min_int then raise Solution.Overflow;
      drop_high_exclusions { bounds with upper = Some (upper - 1) }
    | _ -> bounds

  let contains bounds v =
    Option.fold ~none:true ~some:(fun lower -> lower <= v) bounds.lower
    && Option.fold ~none:true ~some:(fun upper -> v <= upper) bounds.upper

  let normalize bounds =
    let trimmed =
      bounds
      |> drop_low_exclusions
      |> drop_high_exclusions
    in
    check_clearly_contradictory trimmed;
    let excluded = IntSet.filter (contains trimmed) bounds.excluded in
    { trimmed with excluded }

  (* Raises Unsat if bounds cannot be normalized. *)
  let to_clauses key bounds =
    let bounds = normalize bounds in
    let variable = Formula.symbol (I key) in
    match bounds.lower, bounds.upper with
    | Some lower, Some upper when lower = upper ->
      [ Formula.binop Equal variable (Formula.const_int lower) ]
    | lower, upper ->
      let lower_clause =
        Option.map (fun value ->
          Formula.binop Less_than_eq (Formula.const_int value) variable
        ) lower
      and upper_clause =
        Option.map (fun value ->
          Formula.binop Less_than_eq variable (Formula.const_int value)
        ) upper
      and excluded_clauses =
        IntSet.fold (fun value clauses ->
          Formula.not_ (Formula.binop Equal variable (Formula.const_int value))
          :: clauses
        ) bounds.excluded []
      in
      List.filter_map Fun.id [ lower_clause ; upper_clause ] @ excluded_clauses
end

let bound_update = function
  | Formula.Binop (Equal, Key (I key), Const_int value)
  | Formula.Binop (Equal, Const_int value, Key (I key)) ->
    Some (key, Bounds.set_equal value)
  | Formula.Not (Binop (Equal, Key (I key), Const_int value))
  | Formula.Not (Binop (Equal, Const_int value, Key (I key))) ->
    Some (key, Bounds.exclude value)
  | Formula.Binop (Less_than_eq, Const_int value, Key (I key)) ->
    Some (key, Bounds.with_lower value)
  | Formula.Binop (Less_than, Const_int value, Key (I key)) ->
    if value = Int.max_int then raise Solution.Overflow;
    Some (key, Bounds.with_lower (value + 1))
  | Formula.Binop (Less_than_eq, Key (I key), Const_int value) ->
    Some (key, Bounds.with_upper value)
  | Formula.Binop (Less_than, Key (I key), Const_int value) ->
    if value = Int.min_int then raise Solution.Overflow;
    Some (key, Bounds.with_upper (value - 1))
  | _ ->
    None

let rec collect_bounds acc_bounds acc_other = function
  | [] -> acc_bounds, acc_other
  | clause :: rest ->
    match bound_update clause with
    | Some (key, update) ->
      let bounds = update (Bounds.find_bounds key acc_bounds) in
      Bounds.check_clearly_contradictory bounds;
      collect_bounds (Utils.Uid.Map.add key bounds acc_bounds) acc_other rest
    | None ->
      collect_bounds acc_bounds (clause :: acc_other) rest

let tighten_bounds formula =
  let clauses =
    match formula with
    | Formula.And clauses -> clauses
    | formula -> [ formula ]
  in
  let bounds_by_key, other_clauses =
    collect_bounds Utils.Uid.Map.empty [] clauses
  in
  let rec emit acc = function
    | [] -> Formula.and_ acc
    | (key, bounds) :: rest ->
      let clauses = Bounds.to_clauses key bounds in
      emit (List.rev_append clauses acc) rest
  in
  emit other_clauses (Utils.Uid.Map.to_list bounds_by_key)

(* Catches any unsats raised by any utility functions in this module, so those
  exceptions do not escape. *)
let tighten_bounds formula =
  try tighten_bounds formula with
  | Unsat -> Formula.const_bool false
