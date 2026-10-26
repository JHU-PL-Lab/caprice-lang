
let root_dir = Sys.getenv "CTL_TESTDIR"

let make_test (dir : string) : unit Alcotest.test option =
  match Utils.Files.find_caprice_files ~recpaths:false dir with
  | [] -> None
  | ls -> Option.some (
    dir,
    List.sort String.compare ls
    |> List.filter_map Caprice_test.Ctl_semantics.make_test
  )

let () =
  let program_tests =
    root_dir
    |> Utils.Files.all_subdirectories
    |> List.filter_map make_test
  in
  Alcotest.run "test-caprice"
    (Caprice_test.Smt_simplifiers.tests :: program_tests)
