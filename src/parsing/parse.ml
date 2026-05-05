open Lexing

exception Parse_error of exn * int * int * string

module Default = Parser.Make (Param.Standard)

module type PARSER_ENTRY = sig
  type result
  val entry_point : (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> result
end

let handle_parse_error buf f =
  try f ()
  with exn ->
    let curr = buf.lex_curr_p in
    let line = curr.pos_lnum in
    let column = curr.pos_cnum - curr.pos_bol in
    let tok = lexeme buf in
    raise @@ Parse_error (exn, line, column, tok)

module Make(Parser_entry: PARSER_ENTRY) = struct
  let parse_lexbuf (buf : Lexing.lexbuf) : Parser_entry.result =
    handle_parse_error buf @@ fun () ->
    Parser_entry.entry_point Lexer.token buf

  let parse_string (input : string) : Parser_entry.result =
    parse_lexbuf (Lexing.from_string input)

  let parse_program (input : in_channel) : Parser_entry.result =
    parse_lexbuf (Lexing.from_channel input)

  let parse_file (filename : string) : Parser_entry.result =
    In_channel.with_open_bin filename parse_program

  let parse_program_from_argv =
    let open Cmdliner.Term.Syntax in
    let+ src_file =
      let open Cmdliner.Arg in
      required & pos 0 (some' file) None & info [] ~docv:"FILE" ~doc:"Input filename"
    in
    parse_file src_file
end

include Make (struct
  type result = Lang.Ast.statement list
  let entry_point = Default.prog
end)

module Positioned = Make (struct
  type result = Lang.Ast.statement_with_pos list
  let entry_point = Default.prog_with_pos
end)

let parse_stripped (input : string) : Lang.Ast.statement_with_pos list * Lang.Ast.pos_span list =
  let module Ignore = Param.Make_ignore_refine () in
  let module Stripped_parser = Parser.Make (Ignore) in
  let module Stripped_parse = Make (struct
      type result = Lang.Ast.program_with_pos
      let entry_point = Stripped_parser.prog_with_pos
    end)
  in
  let stmts = Stripped_parse.parse_string input in
  stmts, Ignore.positions ()
