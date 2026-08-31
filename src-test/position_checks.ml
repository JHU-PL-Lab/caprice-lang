let parse_position (piece : string) : Lsp.Positions.pos * Lsp.Positions.pos =
  Scanf.sscanf piece "%d:%d-%d:%d" (fun sl sc el ec ->
    (Lsp.Positions.of_1based sl sc, Lsp.Positions.of_1based el ec)
  )

let split_commas s =
  List.map String.trim (String.split_on_char ',' s)

let parse_strings parse s =
  List.map parse (split_commas s)

let parse_positions (s : string) : (Lsp.Positions.pos * Lsp.Positions.pos) list =
  parse_strings parse_position s

let parse_changes (s : string) : Lsp.Protocol.range list =
  List.map (fun (start_pos, end_pos) ->
    { Lsp.Protocol.start_pos ; end_pos }
  ) (parse_positions s)

let parse_spans_from_file (filename : string) : Utils.Pos.Span.t list =
  List.map (fun (_pgm, pos) ->
    pos.Lang.Ast.full
  ) (Parsing.Parse.Positioned.parse_file filename)
