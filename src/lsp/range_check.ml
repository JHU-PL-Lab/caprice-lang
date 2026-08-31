open Lang.Ast

let compute_check_pos (stmts_with_pos : program_with_pos)
    (changes : Protocol.range list) : Utils.Pos.Span.t option =
  match changes with
  | [] -> None
  | first_change :: _ ->
    let target = first_change.start_pos in
    let contains_target span =
      let stmt_end = Positions.of_lexing span.Utils.Pos.Span.ends in
      Positions.geq stmt_end target
    in
    let rec find = function
      | [] -> None
      | (_, pos) :: [] -> Some pos.full
      | (_, prev_pos) :: ((_, next_pos) :: _ as tl) ->
        if contains_target next_pos.full then
          Some prev_pos.full
        else
          find tl
    in
    find stmts_with_pos
  (* TODO: Skip spawning the typechecker for non-semantic edits (e.g., inserting blank lines). *)
