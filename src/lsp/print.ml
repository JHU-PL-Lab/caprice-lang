let format_span (span : Utils.Pos.Span.t) =
  let b = Positions.of_lexing span.begins
  and e = Positions.of_lexing span.ends in
  Printf.sprintf "%d:%d:%d:%d" b.line b.character e.line e.character

let print_pending span =
  Printf.printf "pending:%s\n%!" (format_span span)

let print_splay_error span msg =
  Printf.printf "splay_error:%s:%s\n%!" (format_span span) msg

let print_clear_range span =
  Printf.printf "clear_range:%s\n%!" (format_span span)

let print_answer pos answer =
  let full = format_span pos.Lang.Ast.full
  and small = format_span pos.small in
  match answer with
  | Grammar.Answer.Found_error msg ->
    print_clear_range pos.small;
    Printf.printf "error:%s:%s\n%!" full msg
  | Grammar.Answer.Timeout _ ->
    Printf.printf "timeout:%s\n%!" small
  | Grammar.Answer.Unknown ->
    Printf.printf "unknown:%s\n%!" small
  | Grammar.Answer.Exhausted_pruned ->
    Printf.printf "exhausted_pruned:%s\n%!" small
  | Grammar.Answer.Exhausted ->
    Printf.printf "ok:%s\n%!" small

let print_refinement_warning pos =
  Printf.printf "refinement_warning:%s\n%!" (format_span pos)
