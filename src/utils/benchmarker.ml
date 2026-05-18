let bench_many ~trials tests =
  Benchmark.latencyN (Int64.of_int trials) tests

let bench_many_avg ~trials tests =
  let n = Int64.of_int trials in
  bench_many ~trials tests
  |> List.map (function
    | label, [ t ] ->
      label, (t.Benchmark.wall *. 1_000_000.0 /. Int64.to_float n)
    | label, _ ->
      failwith ("Unexpected latencyN output for " ^ label))
