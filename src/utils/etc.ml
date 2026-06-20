(* et cetera *)

(** [assert_uniq_ctor x y] asserts that the constructors of [x] and [y]
  are not the same, using magic.

  It is expected that this is used after pattern matching on the tuple
  [(x,y)] to assert that all equal constructor cases have been handled.
*)
let[@inline] assert_uniq_ctor (x : 'a) (y : 'a) : unit =
  assert (
    let ctor_id x =
      let o = Obj.repr x in
      if Obj.is_block o then Obj.tag o
      else Obj.magic o  (* immediate constructor index *)
    in
    ctor_id x <> ctor_id y
  )

(** Set minor heap size to 1 million words. *)
let set_1M_minor_heap () =
  Gc.set ({ (Gc.get ()) with minor_heap_size = 1 lsl 20 })
