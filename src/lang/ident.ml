
module T = struct
  type t = Ident of string [@@unboxed]

  let compare (Ident a) (Ident b) =
    String.compare a b

  let equal (Ident a) (Ident b) =
    String.equal a b
end

include T

let to_string (Ident s) = s

include Utils.Set_map.Make_W (T)
