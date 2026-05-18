let fold_until f finish init xs =
  let rec go acc i =
    if i >= Array.length xs then
      finish acc
    else
      match f acc xs.(i) with
      | `Stop x -> x
      | `Continue acc' -> go acc' (i + 1)
  in
  go init 0

let foldi f init xs =
  let acc = ref init in
    Array.iteri (fun i x -> acc := f !acc i x) xs;
  !acc

let foldi_until f finish init xs =
  let rec go acc i =
    if i >= Array.length xs then
      finish acc
    else
      match f acc i xs.(i) with
      | `Stop x -> x
      | `Continue acc' -> go acc' (i + 1)
  in
  go init 0
