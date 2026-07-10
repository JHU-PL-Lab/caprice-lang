
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

  (*
    Trade between the left and right lists so that either:
    - if k exists as a key, then it is at the head of right
    - otherwise all keys in the left are smaller than k and all in the right are
      greater than k

    We do not do this in place to align for a few reasons, which might not all
    be well founded:
    - We avoid the write barrier on every rotation.
    - We can pass the list directly as an argument instead of writing into a
      record field.
    - We can avoid aligning lists that are written to, only those that are read.

    To do it in place would be like this:
      (* in place of the first `aligned` rec call *)
      x.left <- binding :: x.left;
      x.right <- tl_right;
      align k x
    and
      (* in place of the second `aligned` rec call *)
      x.right <- binding :: x.right;
      x.left <- tl_left;
      align k x
    and return `()` instead of `left, right`.
  *)
  let rec aligned k left right =
    match right with
    | (k_right, _) as binding :: tl_right when Key.compare k_right k < 0 ->
      (* right head is too small; it needs to be on the left *)
      aligned k (binding :: left) tl_right
    | _ ->
      begin match left with
      | (k_left, _) as binding :: tl_left when Key.compare k_left k >= 0 ->
        (* left head is too big; it needs to be on the right *)
        aligned k tl_left (binding :: right)
      | _ ->
        left, right
      end

  let align k x =
    let new_left, new_right = aligned k x.left x.right in
    x.left <- new_left;
    x.right <- new_right

  let find_opt k x =
    align k x;
    match x.right with
    | (k', v) :: _ when Key.compare k k' = 0 -> Some v
    | _ -> None

  (*
    Rather than align the old map to point near this new binding, we only align
    the new map. Thus, we do not call `align`.
  *)
  let add k v x =
    let left, right = aligned k x.left x.right in
    let new_right_tail =
      match right with
      | (k', _) :: tl when Key.compare k k' = 0 -> tl
      | ls -> ls
    in
    { left ; right = (k, v) :: new_right_tail }

  let to_list x =
    List.rev_append x.left x.right

  let fold f acc x =
    List.fold_left f acc (to_list x)

  let extend map ~with_ =
    fold (fun acc (k, v) ->
      add k v acc
    ) map with_
end
