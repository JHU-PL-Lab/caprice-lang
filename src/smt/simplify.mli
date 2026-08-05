(** A satisfiability-preserving reduction of a formula. *)
type 'k reduction =
  | Contradiction
  | Reduced of
      { residual : (bool, 'k) Formula.t
      ; extracted : 'k Model.t
      }

(** [reduce formula] repeatedly normalizes integer constraints and substitutes
    implied concrete assignments. Extracted assignments are retained so a
    model for the residual formula can be lifted to a model for FORMULA. *)
val reduce : (bool, 'k) Formula.t -> 'k reduction
