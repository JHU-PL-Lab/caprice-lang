
module Label = struct
  module T = struct
    type t = RecordLabel of Ident.t [@@unboxed]

    let compare (RecordLabel a) (RecordLabel b) =
      Ident.compare a b

    let equal (RecordLabel a) (RecordLabel b) =
      Ident.equal a b
  end

  include T

  let to_ident (RecordLabel id) = id
  let of_ident id = RecordLabel id

  let to_string (RecordLabel id) = Ident.to_string id

  include Utils.Set_map.Make_W (T)
end

type 'a t = 'a Label.Map.t

let empty = Label.Map.empty

let fold (f : Label.t -> 'a -> 'acc -> 'acc) (acc : 'acc) (x : 'a t) : 'acc =
  Label.Map.fold f x acc

let label_set (x : 'a t) : Label.Set.t =
  Label.Map.domain x

module Parsing = struct
  let of_list pair_ls =
    let add_entry acc (k, v) =
      Label.Map.update k (function
        | Some _ -> invalid_arg "Duplicate record entry while parsing."
        | None -> Some v
      ) acc
    in
    List.fold_left add_entry Label.Map.empty pair_ls
end
