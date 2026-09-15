
module type KEY = sig
  type t
  val uid : t -> Utils.Uid.t
end

(*
  Symbols have a phantom 'k (key) parameter. The underlying key is actually a
  Uid.t.
*)
type ('a, 'k) t = ('a, Utils.Uid.t) s
and (_, 'b) s =
  | I : 'b -> (int, 'b) s
  | B : 'b -> (bool, 'b) s

module Make (Key : KEY) = struct
  type nonrec 'a t = ('a, Key.t) t

  let make_int (k : Key.t) : int t =
    I (Key.uid k)

  let make_bool (k : Key.t) : bool t =
    B (Key.uid k)
end
