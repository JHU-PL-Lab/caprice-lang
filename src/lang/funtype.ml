
type mode =
  | Det
  | Nondet

let equal_mode a b =
  match a, b with
  | Det, Det
  | Nondet, Nondet -> true
  | _ -> false

(* merges to the more strict of the two *)
let merge_mode a b =
  match a, b with
  | Nondet, Nondet -> Nondet
  | _ -> Det

let mode_to_string = function
  | Det -> "->"
  | Nondet -> "~>"

type ('dom, 'cod) t = { domain : 'dom ; codomain : 'cod ; mode : mode }
