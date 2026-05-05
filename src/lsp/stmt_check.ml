open Lang.Ast

let disable_annot_check (annot : annot) : annot =
  match annot with
  | ANone -> ANone
  | AType r -> AType { r with do_check = false }

let disable_stmt_check (stmt : statement) : statement =
  match stmt with
  | SLet r -> SLet { r with annot = disable_annot_check r.annot }
  | SLetRec r -> SLetRec { r with annot = disable_annot_check r.annot }

let disable_all_checks (pgm : program_with_pos) : program_with_pos =
  List.map (fun (stmt, span) -> (disable_stmt_check stmt, span)) pgm

let split_on_pos (pgm : program_with_pos) (pos : pos_span)
  : program_with_pos * program_with_pos =
  let before (_, span) = compare_pos_span span pos < 0 in
  List.take_while before pgm, List.drop_while before pgm

(* Produces programs with exactly one check each — the statement at
   [start_pos] onward, in turn. *)
let mk_pgms (pgm : program_with_pos) ~start_pos : (pos_span * program) list =
  let rec mk left right =
    match right with
    | [] -> []
    | (stmt, span) :: rem ->
      let res = mk ((disable_stmt_check stmt, span) :: left) rem in
      (span, List.rev_map fst ((stmt, span) :: left)) :: res
  in
  let prev, rest = split_on_pos pgm start_pos in
  mk (List.rev (disable_all_checks prev)) rest
