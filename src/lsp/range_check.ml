open Lang.Ast

let compute_check_pos (stmts_with_pos : program_with_pos)
    (changes : Protocol.range list) : pos_span option =
  match changes with
  | [] -> None
  | first_change :: _ ->
    let target = first_change.start_pos in
    let contains_target span =
      let stmt_end = Positions.of_lexing span.ends in
      Positions.geq stmt_end target
    in
    let rec find = function
      | [] -> None
      | (_, span) :: [] -> Some span
      | (_, prev_span) :: ((_, next_span) :: _ as tl) ->
        if contains_target next_span then
          Some prev_span
        else
          find tl
    in
    find stmts_with_pos
  (* TODO: Skip spawning the typechecker for non-semantic edits (e.g., inserting blank lines). *)
