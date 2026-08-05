type affine =
  | Const of int
  | Var_plus_const of Utils.Uid.t * int

let add_affine left right =
  match left, right with
  | Const x, Const y ->
    Some (Const (x + y))
  | Var_plus_const (x, c), Const k
  | Const k, Var_plus_const (x, c) ->
    Some (Var_plus_const (x, c + k))
  | Var_plus_const _, Var_plus_const _ -> None

let sub_affine left right =
  match left, right with
  | Const x, Const y ->
    Some (Const (x - y))
  | Var_plus_const (x, c), Const k ->
    Some (Var_plus_const (x, c - k))
  | Const _, Var_plus_const _
  | Var_plus_const _, Var_plus_const _ -> None

let rec affine_from_formula
  : type a k. (a, k) Formula.t -> affine option =
  function
  | Const_int c ->
    Some (Const c)
  | Key (I x) ->
    Some (Var_plus_const (x, 0))
  | Binop (Plus, left, right) ->
    begin
      match affine_from_formula left, affine_from_formula right with
      | Some left, Some right -> add_affine left right
      | _ -> None
    end
  | Binop (Minus, left, right) ->
    begin
      match affine_from_formula left, affine_from_formula right with
      | Some left, Some right -> sub_affine left right
      | _ -> None
    end
  | _ ->
    None

let comparison_from_affines binop left right =
  match left, right with
  | Var_plus_const (x, offset), Const constant ->
    Some (
      Formula.binop binop
        (Formula.symbol (I x))
        (Formula.const_int (constant - offset))
    )
  | Const constant, Var_plus_const (x, offset) ->
    Some (
      Formula.binop binop
        (Formula.const_int (constant - offset))
        (Formula.symbol (I x))
    )
  | Const left, Const right ->
    Some (
      Formula.binop binop
        (Formula.const_int left)
        (Formula.const_int right)
    )
  | Var_plus_const (x, left), Var_plus_const (y, right)
    when Utils.Uid.equal x y ->
    Some (
      Formula.binop binop
        (Formula.const_int left)
        (Formula.const_int right)
    )
  | Var_plus_const _, Var_plus_const _ -> None

let rec linearize formula =
  match formula with
  | Formula.Binop
      (((Equal | Less_than | Less_than_eq) as binop), left, right) ->
    begin
      match affine_from_formula left, affine_from_formula right with
      | Some left, Some right ->
        Option.value
          (comparison_from_affines binop left right)
          ~default:formula
      | _ -> formula
    end
  | Formula.And clauses ->
    Formula.and_ (List.map linearize clauses)
  | Formula.Not inner ->
    Formula.not_ (linearize inner)
  | formula -> formula

module IntSet = Set.Make (Int)

type bounds =
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

let with_equal value bounds =
  bounds
  |> with_lower value
  |> with_upper value

let with_excluded value bounds =
  { bounds with excluded = IntSet.add value bounds.excluded }

let contradictory bounds =
  match bounds.lower, bounds.upper with
  | Some lower, Some upper ->
    lower > upper
    || (lower = upper && IntSet.mem lower bounds.excluded)
  | _ -> false

let rec trim_excluded_lower bounds =
  match bounds.lower with
  | Some lower when IntSet.mem lower bounds.excluded ->
    if lower = Int.max_int then
      None
    else
      trim_excluded_lower { bounds with lower = Some (lower + 1) }
  | _ -> Some bounds

let rec trim_excluded_upper bounds =
  match bounds.upper with
  | Some upper when IntSet.mem upper bounds.excluded ->
    if upper = Int.min_int then
      None
    else
      trim_excluded_upper { bounds with upper = Some (upper - 1) }
  | _ -> Some bounds

let normalize_bounds bounds =
  match trim_excluded_lower bounds with
  | None -> None
  | Some bounds ->
    begin
      match trim_excluded_upper bounds with
      | None -> None
      | Some bounds when contradictory bounds -> None
      | Some bounds ->
        let within_bounds value =
          Option.fold ~none:true ~some:(fun lower -> lower <= value) bounds.lower
          && Option.fold ~none:true ~some:(fun upper -> value <= upper) bounds.upper
        in
        Some {
          bounds with
          excluded = IntSet.filter within_bounds bounds.excluded;
        }
    end

let clauses_from_bounds key bounds =
  let variable = Formula.symbol (I key) in
  match bounds.lower, bounds.upper with
  | Some lower, Some upper when lower = upper ->
    [ Formula.binop Equal variable (Formula.const_int lower) ]
  | lower, upper ->
    let lower_clause =
      Option.map (fun value ->
        Formula.binop Less_than_eq (Formula.const_int value) variable
      ) lower
    in
    let upper_clause =
      Option.map (fun value ->
        Formula.binop Less_than_eq variable (Formula.const_int value)
      ) upper
    in
    let excluded_clauses =
      IntSet.fold (fun value clauses ->
        Formula.not_ (Formula.binop Equal variable (Formula.const_int value))
        :: clauses
      ) bounds.excluded []
    in
    List.filter_map Fun.id [ lower_clause ; upper_clause ] @ excluded_clauses

let tighten_bounds formula =
  let clauses =
    match formula with
    | Formula.And clauses -> clauses
    | formula -> [ formula ]
  in
  let rec collect bounds_by_key other_clauses = function
    | [] ->
      let rec emit acc = function
        | [] -> Formula.and_ (List.rev other_clauses @ List.rev acc)
        | (key, bounds) :: rest ->
          begin
            match normalize_bounds bounds with
            | None -> Formula.const_bool false
            | Some bounds ->
              emit
                (List.rev_append (clauses_from_bounds key bounds) acc)
                rest
          end
      in
      emit [] (Utils.Uid.Map.to_list bounds_by_key)
    | clause :: rest ->
      let continue key update =
        let bounds = update (find_bounds key bounds_by_key) in
        if contradictory bounds then
          Formula.const_bool false
        else
          collect
            (Utils.Uid.Map.add key bounds bounds_by_key)
            other_clauses
            rest
      in
      match clause with
      | Formula.Binop (Equal, Key (I key), Const_int value)
      | Formula.Binop (Equal, Const_int value, Key (I key)) ->
        continue key (with_equal value)
      | Formula.Not (Binop (Equal, Key (I key), Const_int value))
      | Formula.Not (Binop (Equal, Const_int value, Key (I key))) ->
        continue key (with_excluded value)
      | Formula.Binop (Less_than_eq, Const_int value, Key (I key)) ->
        continue key (with_lower value)
      | Formula.Binop (Less_than, Const_int value, Key (I key)) ->
        if value = Int.max_int then
          Formula.const_bool false
        else
          continue key (with_lower (value + 1))
      | Formula.Binop (Less_than_eq, Key (I key), Const_int value) ->
        continue key (with_upper value)
      | Formula.Binop (Less_than, Key (I key), Const_int value) ->
        if value = Int.min_int then
          Formula.const_bool false
        else
          continue key (with_upper (value - 1))
      | clause ->
        collect bounds_by_key (clause :: other_clauses) rest
  in
  collect Utils.Uid.Map.empty [] clauses
