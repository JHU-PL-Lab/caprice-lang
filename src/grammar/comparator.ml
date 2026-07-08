(*
  This module provides helpers for symbolic comparison.
*)

module Make (M : Utils.Types.MONAD) = struct
  type t = bool Cdata.t M.m

  let make b = M.return (Cdata.of_bool b)

  let ( let- ) x f =
    let ( let* ) = M.bind in
    let* x in
    match x with
    | (_, Smt.Formula.Const_bool false) -> M.return x
    | (a, s) ->
      let* (b, s') = f () in
      M.return (a && b, Smt.Formula.and_ [ s' ; s ])

  (* Short circuit if the concrete boolean is false. Think like "map" *)
  let ( let= ) x f =
    if x then f () else make false

  let rec reduce_lists (f : 'a -> 'a -> t) (x : 'a list) (y : 'a list) : t =
    match x, y with
    | [], [] -> make true
    | [], _ | _, [] -> make false
    | hdx :: xs, hdy :: ys ->
      let- () = f hdx hdy in
      reduce_lists f xs ys
end

include Make (Utils.Identity.Monad)
