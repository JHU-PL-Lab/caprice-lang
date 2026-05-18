module T = struct
  type t = Uid of int [@@unboxed]

  let compare (Uid a) (Uid b) =
    Int.compare a b

  let equal (Uid a) (Uid b) =
    Int.equal a b
end

include T

let[@inline] of_int i = Uid i
let[@inline] to_int (Uid i) = i

let counter = Counter.create ()

let make_new () = Uid (Counter.next counter)

include Set_map.Make_W (T)
