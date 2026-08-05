
let root_dir = Sys.getenv "CTL_TESTDIR"

let ls_dir dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.map (Filename.concat dir)

let all_subdirectories (dir : string) : string list =
  let[@tail_mod_cons] rec go = function
    | [] -> []
    | dir :: worklist when Sys.is_directory dir ->
      dir :: go (ls_dir dir @ worklist)
    | _ :: worklist ->
      go worklist
  in
  go [ dir ]

let find_caprice_files (dir : string) : string list =
  ls_dir dir
  |> List.filter (fun file -> Filename.check_suffix file "caprice")

let make_test (dir : string) : unit Alcotest.test option =
  match find_caprice_files dir with
  | [] -> None
  | ls -> Option.some (
    dir,
    List.sort String.compare ls
    |> List.filter_map Caprice_test.Ctl_semantics.make_test
  )

let () =
  let program_tests =
    root_dir
    |> all_subdirectories
    |> List.filter_map make_test
  in
  Alcotest.run "test-caprice"
    (Caprice_test.Smt_simplifiers.tests :: program_tests)
