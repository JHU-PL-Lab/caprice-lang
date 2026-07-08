
type t = Step of int [@@unboxed]

let compare (Step a) (Step b) =
  Int.compare a b

let zero = Step 0

(** [dummy] is gauranteed to be smaller than all steps. *)
let dummy = Step (-1)

let[@inline] next (Step i : t) : t =
  Step (i + 1)

let[@inline] (>) (Step a) (Step b) = a > b

let[@inline] uid (Step i) = Utils.Uid.of_int i

let argv_step_conv =
  Cmdliner.Arg.Conv.make ()
    ~docv:"STEP"
    ~parser:(fun s ->
      match int_of_string_opt s with
      | Some i -> Ok (Step i)
      | None -> Error "Expected integer step"
      )
    ~pp:(fun formatter (Step i) -> Format.pp_print_int formatter i)
