
module type KEY = sig
  type t
  val uid : t -> Utils.Uid.t
end

(* Symbols have a phantom 'k (key) parameter. The underlying
  key is actually a Uid.t *)
type ('a, 'k) t = ('a, Utils.Uid.t) s
and (_, 'b) s =
  | I : 'b -> (int, 'b) s
  | B : 'b -> (bool, 'b) s

let compare (type a) (x : (a, 'k) t) (y : (a, 'k) t) : int =
  match x, y with
  | I xi, I yi
  | B xi, B yi -> Utils.Uid.compare xi yi

let equal (type a) (x : (a, 'k) t) (y : (a, 'k) t) : bool =
  match x, y with
  | I xi, I yi
  | B xi, B yi -> Utils.Uid.equal xi yi

module Make (Key : KEY) = struct
  type nonrec 'a t = ('a, Key.t) t

  let make_int (k : Key.t) : int t =
    I (Key.uid k)

  let make_bool (k : Key.t) : bool t =
    B (Key.uid k)
end

(** [to_uid symbol] extracts the [Uid.t] key from SYMBOL. *)
let to_uid (type a) (key : (a, 'k) t) =
  match key with
  | B uid
  | I uid -> uid

module AsciiSymbol = struct
  include Make (struct
    type t = char
    let uid t = t |> Char.code |> Utils.Uid.of_int
  end)

  let to_string uid =
    uid
    |> Utils.Uid.to_int
    |> Char.chr
    |> String.of_char
end

