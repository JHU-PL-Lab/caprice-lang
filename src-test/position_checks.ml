let parse_position (piece : string) : Lsp.Positions.pos * Lsp.Positions.pos =
  Scanf.sscanf piece "%d:%d-%d:%d"
    (fun sl sc el ec ->
      (Lsp.Positions.of_1based sl sc, Lsp.Positions.of_1based el ec))

let parse_positions (s : string) : (Lsp.Positions.pos * Lsp.Positions.pos) list =
  s
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.map parse_position

let parse_int_list (s : string) : int list =
  s
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.map int_of_string

let mk_range ((s, e) : Lsp.Positions.pos * Lsp.Positions.pos) : Lsp.Protocol.range =
  { start_pos = s ; end_pos = e }

let parse_changes (s : string) : Lsp.Protocol.range list =
  s
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.map parse_position
  |> List.map mk_range

let parse_spans_from_file (filename : string) : Lang.Ast.pos_span list =
  filename
  |> Parsing.Parse.Positioned.parse_file
  |> List.map snd
