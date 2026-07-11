
module Make (Key : Map.OrderedType) = struct
  type 'a binding = Key.t * 'a

  (*
    Persistent map that keeps its pointer at the last addition or lookup
    because in its use case, it is most common to read and write very near
    the last access.

    The left are keys strictly smaller than the cursor, in descending order.
    The right are keys greater than or equal to the cursor, in ascending order.

    The left keys are therefore all smaller than the right keys.
  *)
  type 'a t =
    { mutable left : 'a binding list
    ; mutable right : 'a binding list }

  (* Must be created fresh to avoid weak type variables *)
  let empty () = { left = [] ; right = [] }

  (** Shift from the right list onto the left list while [key] is at least as
    large as the head of the right list.
    If [key] is strictly less than the right list, or the right list is empty,
    then continues with [with_] instead of shifting. *)
  let rec try_shift_onto_left_until ~with_ ~key left right =
    match right with
    | (k_right, v) as binding :: tl_right ->
      let c = Key.compare k_right key in
      if c = 0 then
        (* aligned exactly on the desired key *)
        left, Some v, right
      else if c < 0 then
        (* right head is too small; it needs to be on the left. Shift left
          and stop once the key is hit. *)
        shift_onto_left_until ~key (binding :: left) tl_right
      else
      with_ ~key left right
    | [] ->
      with_ ~key left right

  (** Shift from the right list onto the left list while [key] is still smaller
    than the right list.
    Precondition: [key] is at least as large as the head of the right list. *)
  and shift_onto_left_until ~key left right =
    try_shift_onto_left_until ~key left right
      ~with_:(fun ~key:_ l r -> l, None, r)

  (** Shift from the left list onto the right list while [key] is smaller than
    the head of the left list.
    Precondition: [key] is at least as small as the head of the left list. *)
  let rec shift_onto_right_until ~key left right =
    match left with
    | (k_left, v) as binding :: tl_left ->
      let c = Key.compare k_left key in
      if c = 0 then
        (* align the key to be on the right *)
        tl_left, Some v, binding :: right
      else if c > 0 then
        (* left head is small; shift from right until we hit the key *)
        shift_onto_right_until ~key tl_left (binding :: right)
      else
        left, None, right
    | [] ->
      left, None, right

  (**
    Trade between the left and right lists so that either:
    - if [key] exists as a key, then it is at the head of right
    - otherwise all keys in the left are smaller than [key] and all in the right
      are greater than [key]

    This is done by trying to shift from right onto left. But if the right is
    already above the key, then the key may be contained in the left, so we
    shift from left onto right instead.
  *)
  let align ~key left right =
    try_shift_onto_left_until ~key left right ~with_:shift_onto_right_until

  let find_opt key x =
    let left, v_opt, right = align ~key x.left x.right in
    x.left <- left;
    x.right <- right;
    v_opt

  (*
    Rather than align the old map to point near this new binding, we only align
    the new map. We do not mutate the old map to point near this addition.
  *)
  let add key v x =
    let left, v_opt, right = align ~key x.left x.right in
    match v_opt with
    | Some _ -> { left ; right = (key, v) :: List.tl right }
    | None -> { left ; right = (key, v) :: right }

  let to_list x =
    List.rev_append x.left x.right

  let fold f acc x =
    List.fold_left f acc (to_list x)

  let extend map ~with_ =
    fold (fun acc (k, v) ->
      add k v acc
    ) map with_
end
