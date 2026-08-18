
module Q = Psq.Make (Target) (Priority)

type t = BFS of Q.t [@@unboxed]

let empty : t = BFS Q.empty

let push_one (BFS q : t) (target : Target.t) : t =
  let priority = Target.priority target in
  BFS (Q.push target priority q)

let push_list (x : t) (ls : Target.t list) : t =
  List.fold_left push_one x ls

(* let remove (BFS q : t) (target : Target.t) : t =
  BFS (Q.remove target q) *)

(**
  Pop a target, which then must be solved and have its input environment
  updated before being used for evaluation. The input environment in any target
  popped here is not a solution to its constraints.
*)
let pop (BFS q : t) : (Target.t * t) option =
  match Q.pop q with
  | Some ((target, _), t) -> Some (target, BFS t)
  | None -> None

(* contains only the empty target *)
let initial : t =
  push_one empty Target.empty
