
(*
  We will use a zipper map that has fast reads and writes nearest the last read
  or write because inputs are very often read and written in increasing order.
  Inputs are keyed by how many steps the program has taken, which is increasing.
  It only takes exactly one traversal to reset at the start of the program, and
  otherwise the actions are cheap.
*)
module UMap = Utils.Zipper_map.Make (Utils.Uid)

module Make (K : Smt.Symbol.KEY) = struct
  type t = Input.t UMap.t

  let empty : t = UMap.empty ()

  (* Propagates failing extraction. Is None if the key doesn't exist at all *)
  let find (type a) (kind : a Input.Kind.t) (key : K.t) (m : t) : a option =
    Option.map (Input.extract_exn kind) (UMap.find_opt (K.uid key) m)

  let add (type a) (kind : a Input.Kind.t) (key : K.t) (input : a) (m : t) : t =
    UMap.add (K.uid key) (
      match kind with
      | Input.Kind.KBool -> Input.IBool input
      | KInt -> IInt input
      | KTag -> ITag input
    ) m

  let extend = UMap.extend

  let to_string (m : t) : string =
    let make_mapping (uid, input) =
      Printf.sprintf "%d |-> %s" (Utils.Uid.to_int uid) (Input.to_string input)
    in
    let body =
      m
      |> UMap.to_list
      |> List.map make_mapping
      |> String.concat " ; "
    in
    Printf.sprintf "{ %s }" body

  let of_model (model : K.t Smt.Model.t) : t =
    List.fold_left (fun acc uid ->
      let v =
        match model.value (I uid) with
        | Some i -> Input.IInt i
        | None -> IBool (Option.get (model.value (B uid)))
      in
      UMap.add uid v acc
    ) empty model.domain
end

include Make (Stepkey)
