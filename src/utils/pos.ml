
type t = Lexing.position

module Span = struct
  type nonrec t = { begins : t ; ends : t }

  let compare a b =
    match Int.compare a.begins.pos_cnum b.begins.pos_cnum with
    | 0 -> Int.compare a.ends.pos_cnum b.ends.pos_cnum
    | cmp -> cmp

  let equal a b = compare a b = 0

  let dummy =
    { begins = Lexing.dummy_pos ; ends = Lexing.dummy_pos }
end
