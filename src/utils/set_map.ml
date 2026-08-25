
(* Make weight-balanced set and map modules. *)
module Make_W (K : Baby.OrderedType) = struct
  module B = Baby.W.Make (K)

  (* We could make this faster by actually walking the tree. However
    that would require I modify the source code of Baby. *)
  let random_from_seq ~size seq =
    let n = ref size in
    Seq.find (fun _ ->
      let i = Random.int !n in
      n := !n - 1;
      i = 0
    ) seq

  module Map = struct
    include B.Map
    let domain = B.domain

    let random_binding_opt (m : 'a t) : (K.t * 'a) option =
      random_from_seq ~size:(cardinal m) (to_seq m)

    let extend (base : 'a t) ~(with_ : 'a t) : 'a t =
      union (fun _ _ v -> Some v) base with_

    let mapiM (module M : Types.INDEXED_MONAD) (f : K.t -> 'a -> ('b, 'i) M.m)
        (x : 'a t) : ('b t, 'i) M.m =
      fold (fun k a s ->
        M.bind s (fun acc ->
          M.bind (f k a) (fun b ->
            M.return (add k b acc)
          )
        )
      ) x (M.return empty)

    let mapM (module M : Types.INDEXED_MONAD) (f : 'a -> ('b, 'i) M.m)
        (x : 'a t) : ('b t, 'i) M.m =
      mapiM (module M) (fun _ a -> f a) x

    let list_map (f : key -> 'a -> 'b) (t : 'a t) : 'b list =
      let rec aux enum =
        match Enum.head_opt enum with
        | Some (k, a) -> f k a :: aux (Enum.tail enum)
        | None -> []
      in
      aux (Enum.enum t)
  end

  module Set = struct
    include B.Set

    let random_elt_opt (s : t) : elt option =
      random_from_seq ~size:(cardinal s) (to_seq s)

    let list_map (f : elt -> 'b) (t : t) : 'b list =
      let rec aux enum =
        match Enum.head_opt enum with
        | Some a -> f a :: aux (Enum.tail enum)
        | None -> []
      in
      aux (Enum.enum t)
  end
end
