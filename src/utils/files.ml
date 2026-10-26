
let ls_dir dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.map (Filename.concat dir)
  |> List.sort String.compare

let is_caprice_file fname = Filename.check_suffix fname "caprice"

let all_subdirectories (dir : string) : string list =
  let[@tail_mod_cons] rec go = function
    | [] -> []
    | dir :: worklist when Sys.is_directory dir ->
      dir :: go (ls_dir dir @ worklist)
    | _ :: worklist ->
      go worklist
  in
  go [ dir ]

let find_all_files ~(recpaths : bool) ~(filter : string -> bool)
    (root : string) : string list =
  if not (Sys.is_directory root) then
    List.filter filter [root]
  else
    let dirs =
      if recpaths then
        all_subdirectories root
      else
        [ root ]
    in
    List.concat_map (fun dir ->
      List.filter filter (ls_dir dir)
    ) dirs

let find_caprice_files ~recpaths root =
  find_all_files ~recpaths ~filter:is_caprice_file root
