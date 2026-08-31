(** Integer-specific formula normalization. *)

(** [tighten_bounds formula] combines unary integer bounds and disequalities in
    a conjunction. It emits [false] for contradictory bounds and an equality
    when exactly one value remains. *)
val tighten_bounds : (bool, 'k) Formula.t -> (bool, 'k) Formula.t
