(** Integer-specific formula normalization. *)

(** [linearize formula] simplifies [formula] by evaluating basic linear
    operations if any exist in [formula]. *)
val linearize : (bool, 'k) Formula.t -> (bool, 'k) Formula.t

(** [tighten_bounds formula] combines unary integer bounds and disequalities in
    a conjunction. It emits [false] for contradictory bounds and an equality
    when exactly one value remains. *)
val tighten_bounds : (bool, 'k) Formula.t -> (bool, 'k) Formula.t
