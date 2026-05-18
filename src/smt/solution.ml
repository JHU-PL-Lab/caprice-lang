type 'k t =
  | Sat of 'k Model.t
  | Unknown
  | Unsat

let merge (x : 'k t) (y : 'k t) : 'k t =
  match x, y with
  | Unsat, _ | _, Unsat -> Unsat
  | Unknown, _ | _, Unknown -> Unknown
  | Sat m1, Sat m2 -> Sat (Model.merge m1 m2)

let to_string
  (type k)
  ~(key : Model.key -> string)
  (solution : k t)
  : string =
  match solution with
  | Unknown -> "\"Unknown\""
  | Unsat -> "\"Unsat\""
  | Sat model -> Model.to_string model ~key
