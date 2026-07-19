
module T = struct
  type t = Stepkey of Step.t [@@unboxed]

  let compare (Stepkey a) (Stepkey b) =
    Step.compare a b

  let[@inline] uid (Stepkey step) = Step.uid step
end

include T

let step (Stepkey step) = step

module Symb = Smt.Symbol.Make (T)

let int_symbol step = Smt.Formula.symbol (Symb.make_int (Stepkey step))

let bool_symbol step = Smt.Formula.symbol (Symb.make_bool (Stepkey step))
