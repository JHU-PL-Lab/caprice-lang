module type SET = sig
  include Baby.W.Set.S

  val random_elt_opt : t -> elt option
  val list_map : (elt -> 'b) -> t -> 'b list
end

module type MAP = sig
  include Baby.W.Map.S

  val random_binding_opt : 'a t -> (key * 'a) option
  val extend : 'a t -> with_:'a t -> 'a t
end

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

    (** [extend t ~with_] is the union of T and WITH_, where
        shared keys their values replaced with the value from WITH_ *)
    let extend (t : 'a t) ~(with_ : 'a t) : 'a t =
      union (fun _ _ new_v -> Some new_v) t with_
  end

  module Set = struct
    include B.Set

    let random_elt_opt (s : t) : elt option =
      random_from_seq ~size:(cardinal s) (to_seq s)

    let list_map (f : elt -> 'b) (t : t) : 'b list =
      let[@tail_mod_cons] rec aux enum =
        match Enum.head_opt enum with
        | Some a -> f a :: aux (Enum.tail enum)
        | None -> []
      in
      aux (Enum.enum t)
  end
end

