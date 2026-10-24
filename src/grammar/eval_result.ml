
open Lang

type t =
  (* errors *)
  | Refutation of Val.any * Val.tval
  | Mismatch of string
  | Unbound_variable of Ident.t
  | Assert_false
  (* finish without a value or an error *)
  | Reach_max_step of Step.t
  | Confirmation
  | Vanish
  (* finish to value *)
  | Done

let to_outcome = function
  (* error cases *)
  | Refutation (v, t) -> Outcome.Found_error (Val.Error_messages.refutation v t)
  | Mismatch msg -> Found_error msg
  | Unbound_variable Ident id -> Found_error ("Unbound variable: " ^ id)
  | Assert_false -> Found_error "Failed assertion"
  (* stopped early but may have finished to a value if we let it run longer *)
  | Reach_max_step _step -> Exhausted_pruned
  (* stopped without a value but was not cut short *)
  | Confirmation -> Exhausted
  | Vanish -> Exhausted
  (* finished to a value *)
  | Done -> Exhausted

(* stop on errors *)
let is_signal_to_stop res = Outcome.is_error @@ to_outcome res
