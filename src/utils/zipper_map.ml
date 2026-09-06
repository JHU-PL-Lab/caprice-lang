
module Make (Key : Map.OrderedType) = struct
  type 'a binding = Key.t * 'a

  (*
    Persistent map that keeps its pointer at the last addition or lookup
    because in its use case, it is most common to read and write very near the
    last access. This map is fastest if each read is followed by a read of the
    next greatest key after the previous read, and each key added is the
    greatest key.

    The left are keys smaller than or equal to the cursor, in descending order.
    The right are keys strictly greater than the cursor, in ascending order.

    The left keys are therefore all strictly smaller than the right keys.
  *)
  type 'a t =
    { mutable left : 'a binding list
    ; mutable right : 'a binding list }

  (* Must be created fresh to avoid weak type variables *)
  let empty () = { left = [] ; right = [] }

  (** Move the cursor right by shifting from the right list onto the left list
    while [key] greater than or equal to the head of the right list.
    If [key] is strictly less than the right list, or the right list is empty,
    then continues with [with_] instead of shifting. *)
  let rec try_move_cursor_right_until ~with_ ~key left right =
    match right with
    | (k_right, v) as binding :: tl_right ->
      let c = Key.compare k_right key in
      if c = 0 then
        (* Aligned exactly. The desired key will be put on the left. *)
        binding :: left, Some v, tl_right
      else if c < 0 then
        (* Right head is smaller than the cursor; it needs to be on the left of
          the cursor. Keep moving right until the key is found. *)
        move_cursor_right_until ~key (binding :: left) tl_right
      else
        with_ ~key left right
    | [] ->
      with_ ~key left right

  (** Move the cursor right until [key] is exactly the head of the left list or
    until it is between the lists. *)
  and move_cursor_right_until ~key left right =
    try_move_cursor_right_until ~key left right
      ~with_:(fun ~key:_ l r -> l, None, r)

  (** Move the cursor left until [key] is exactly the head of the left list or
    until it is between the lists. *)
  let rec move_cursor_left_until ~key left right =
    match left with
    | (k_left, v) as binding :: tl_left ->
      let c = Key.compare k_left key in
      if c = 0 then
        (* Found the key; keep it on the left *)
        left, Some v, right
      else if c > 0 then
        (* Left head is still greater than the cursor; continue shifting. *)
        move_cursor_left_until ~key tl_left (binding :: right)
      else
        left, None, right
    | [] ->
      left, None, right

  (**
    Trade between the left and right lists so that either:
    - if [key] exists as a key, then it is at the head of left
    - otherwise all keys in the left are smaller than [key] and all in the right
      are greater than [key].

    Returns the optional value associated with the key. If this value is
    [Some v], then the left is not empty ([(key, v)] is the head of the left
    returned list).

    This is done by trying to to move the cursor right, first. If the right is
    already greater than the key, then the key could possibly be in the left, so
    if moving right fails, then we instead move left.
  *)
  let align ~key left right =
    try_move_cursor_right_until ~key left right ~with_:move_cursor_left_until

  (*
    Notice that after calling find_opt, the cursor is aligned so that the next
    greatest key is exactly at the head of the right list, which is where
    alignment begins, so successive calls with successively greater keys (that
    all exist in the map) are extremely cheap. That is, the map is pre-shifted
    for the next lookup.
  *)
  let find_opt key x =
    let left, v_opt, right = align ~key x.left x.right in
    if not (x.left == left) then begin
      (* Something changed, so update the cursor. The right is unchanged if and
        only if the left is unchanged, so checking the left is sufficient. *)
      x.left <- left;
      x.right <- right
    end;
    v_opt

  (*
    Rather than align the old map to point near this new binding, we only align
    the new map. We do not mutate the old map to point near this addition
    because we do not expect the next lookup or addition to the old map to be
    near this one since two extensions of the same map are likely independent.
  *)
  let add key v x =
    let left, v_opt, right = align ~key x.left x.right in
    match v_opt with
    | Some _ -> { left = List.tl left ; right = (key, v) :: right }
    | None -> { left ; right = (key, v) :: right }

  let to_list x =
    List.rev_append x.left x.right

  let fold_left f acc x =
    let left_res = List.fold_right (fun a acc -> f acc a) x.left acc in
    List.fold_left f left_res x.right

  let extend map ~with_ =
    fold_left (fun acc (k, v) -> add k v acc) map with_
end
