
open Lexing

exception Parse_error of string * int * int * string * string

let propagate_parse_error buf filename f =
  try f ()
  with exn ->
    let curr = buf.lex_curr_p in
    let line = curr.pos_lnum in
    let column = curr.pos_cnum - curr.pos_bol in
    let tok = lexeme buf in
    raise @@ Parse_error (Printexc.to_string exn, line, column, tok, filename)

let has_test filename =
  In_channel.with_open_bin filename @@ fun ic ->
    ic
    |> Lexing.from_channel
    |> Ctl_lexer.is_test

let parse_test_header (filename : string) : Ctl_ast.t option =
  if not (has_test filename) then
    None
  else begin
    In_channel.with_open_bin filename @@ fun ic ->
      let buf = Lexing.from_channel ic in
      propagate_parse_error buf filename @@ fun () ->
        Some (Ctl_parser.ctl_script Ctl_lexer.token buf)
  end
