
let make_one ~options fname =
  try
    let pgm = Parsing.Parse.parse_file fname in
    let run pgm =
      match
        Concolic.Loop.begin_ceval pgm
          ~print_outcome:false
          ~options
      with
      | Grammar.Outcome.Timeout _ -> assert false
      | _ -> ()
    in
    Some (Filename.basename fname, run, pgm)
  with
  | Parsing.Parse.Parse_error _ -> None

let bench_throughput ~sec tests =
  Benchmark.throughputN sec tests

let bench_trials ~trials tests =
  Benchmark.latencyN (Int64.of_int trials) tests

let bench_main =
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "bench") @@
  let open Cmdliner.Term.Syntax in
  let+ paths = Cmdliner.Arg.(
      non_empty & pos_all file [] &
      info [] ~docv:"PATH" ~doc:"Input files or directories"
    )
  and+ recpaths = Cmdliner.Arg.(
      value & flag &
      info ["recpaths"] ~doc:"Recursively search directories for files"
    )
  and+ options = Concolic.Options.of_argv
  and+ sec = Cmdliner.Arg.(
      value & opt int 4
      & info ["sec"] ~docv:"SECONDS"
      ~doc:"Minumum benchmarking time, in seconds. Ignored if trials are given"
    )
  and+ trials = Cmdliner.Arg.(
      value & opt (some int) None
      & info ["trials"] ~docv:"TRIALS" ~doc:"Number of trials"
    )
  in
  let tests =
    paths
    |> List.concat_map (Utils.Files.find_caprice_files ~recpaths)
    |> List.filter_map (make_one ~options)
  in
  let _res =
    match trials with
    | Some n -> bench_trials ~trials:n tests
    | None -> bench_throughput ~sec tests
  in
  ()

let () =
  Utils.Etc.set_1M_minor_heap ();
  match Cmdliner.Cmd.eval_value' bench_main with
  | `Ok _ -> ()
  | `Exit i -> exit i
