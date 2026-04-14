let read_exactly ic n =
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  Bytes.to_string buf

let process_one_change ~(options : Concolic.Options.t) =
  let packet_text =
    stdin
    |> input_line
    |> int_of_string
    |> read_exactly stdin
  in
  begin match Lsp.Protocol.parse_checker_packet packet_text with
  | Ok packet -> Lsp.Main_loop.run_typecheck ~options packet
  | Error msg -> Printf.printf "protocol_error:%s\n%!" msg
  end;
  Printf.printf "done\n%!"

let rec server_loop ~(options : Concolic.Options.t) () =
  try
    process_one_change ~options;
    server_loop ~options ()
  with
  | End_of_file -> ()

let caprice_typecheck_lsp_main =
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "caprice_typecheck_lsp") @@
  let open Cmdliner.Term.Syntax in
  let+ options = Concolic.Options.of_argv in
  server_loop ~options ()

let () =
  match Cmdliner.Cmd.eval_value' caprice_typecheck_lsp_main with
  | `Ok _ -> ()
  | `Exit i -> exit i
