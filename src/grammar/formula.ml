
type k = Stepkey.t

type 'a t = ('a, Stepkey.t) Smt.Formula.t

include (Smt.Formula : Smt.Formula.S with type ('a, 'k) t := 'a t)

let trivial = const_bool true
