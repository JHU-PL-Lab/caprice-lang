
module Make (K : Smt.Symbol.KEY) = struct
  type t = Input.t Utils.Uid.Map.t

  let empty : t = Utils.Uid.Map.empty

  (* Propagates failing extraction. Is None if the key doesn't exist at all *)
  let find (type a) (kind : a Input.Kind.t) (key : K.t) (m : t) : a option =
    Option.map (Input.extract_exn kind) (Utils.Uid.Map.find_opt (K.uid key) m)

  let add (type a) (kind : a Input.Kind.t) (key : K.t) (input : a) (m : t) : t =
    Utils.Uid.Map.add (K.uid key) (
      match kind with
      | Input.Kind.KBool -> Input.IBool input
      | KInt -> IInt input
      | KTag -> ITag input
    ) m

  let extend (base_map : t) (extending_map : t) : t =
    Utils.Uid.Map.union (fun _ _ v -> Some v)
      base_map extending_map

  let to_string (m : t) : string =
    "{ " ^
      ( Utils.Uid.Map.to_list m
      |> List.map (fun (uid, input) ->
        string_of_int (Utils.Uid.to_int uid) ^ " |-> " ^ Input.to_string input
        )
      |> String.concat " ; ")
    ^ "}"

  let of_model (model : K.t Smt.Model.t) : t =
    List.fold_left (fun acc key ->
      match key with
      | Smt.Model.Bool_key sym -> (
        match model.value (B sym) with
        | Some b ->
            let uid = Smt.Symbol.to_uid (B sym) in
            Utils.Uid.Map.add uid (Input.IBool b) acc
        | None -> acc
      )
      | Smt.Model.Int_key sym -> (
        match model.value (I sym) with
        | Some i ->
          let uid = Smt.Symbol.to_uid (I sym) in
          Utils.Uid.Map.add uid (Input.IInt i) acc
        | None -> acc
      )
    ) empty model.domain
end

include Make (Stepkey)
