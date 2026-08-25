
(* Raised when an SMT formula has a solution that must overflow, which is out of
  scope of this project, so failure is fine. *)
exception Overflow

type 'k t =
  | Sat of 'k Model.t
  | Unknown
  | Unsat

let merge (x : 'k t) (y : 'k t) : 'k t =
  match x, y with
  | Unsat, _ | _, Unsat -> Unsat
  | Unknown, _ | _, Unknown -> Unknown
  | Sat m1, Sat m2 -> Sat (Model.merge m1 m2)
