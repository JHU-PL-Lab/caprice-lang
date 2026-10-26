
type t = Lexing.position

let compare a b =
  Int.compare a.Lexing.pos_cnum b.Lexing.pos_cnum

module Span = struct
  type nonrec t = { begins : t ; ends : t }

  let compare a b =
    match compare a.begins b.begins with
    | 0 -> compare a.ends b.ends
    | cmp -> cmp

  let dummy =
    { begins = Lexing.dummy_pos ; ends = Lexing.dummy_pos }
end
