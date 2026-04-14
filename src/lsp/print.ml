let format_span (span : Lang.Ast.pos_span) =
  let b = Positions.of_lexing span.begins in
  let e = Positions.of_lexing span.ends in
  Printf.sprintf "%d:%d:%d:%d" b.line b.character e.line e.character

let print_pending span =
  Printf.printf "pending:%s\n%!" (format_span span)

let print_splay_error span msg =
  Printf.printf "splay_error:%s:%s\n%!" (format_span span) msg

let print_answer span answer =
  let pos = format_span span in
  match answer with
  | Grammar.Answer.Found_error msg ->
    Printf.printf "error:%s:%s\n%!" pos msg
  | Grammar.Answer.Timeout _ ->
    Printf.printf "timeout:%s\n%!" pos
  | Grammar.Answer.Unknown ->
    Printf.printf "unknown:%s\n%!" pos
  | Grammar.Answer.Exhausted_pruned ->
    Printf.printf "exhausted_pruned:%s\n%!" pos
  | Grammar.Answer.Exhausted ->
    Printf.printf "ok:%s\n%!" pos