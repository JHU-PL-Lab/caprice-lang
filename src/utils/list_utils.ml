
let[@specialise] rec fold_left_until f finish acc ls =
  match ls with
  | [] -> finish acc
  | hd :: tl ->
    match f acc hd with
    | `Stop x -> x
    | `Continue a -> fold_left_until f finish a tl
