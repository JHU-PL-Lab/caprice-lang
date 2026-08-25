
type t = int ref

let create () : t = ref 0

let next (x : t) : int = let y = !x in incr x; y

let get (x : t) : int = !x

let reset (x : t) : unit = x := 0
