module M = Concolic.Loop.Make (Scheduler.Pause_effect)

let splay_check ~options pgm =
  M.begin_ceval ~print_outcome:false ~options:{ options with splay = Splay_only } pgm

let normal_check ~options pgm =
  M.begin_ceval ~print_outcome:false ~options:{ options with splay = Never_splay } pgm

let handle_fallback ~options ~refinement_positions (span : Lang.Ast.pos_span)
  pgm stripped_pgm : Scheduler.r =
  let refinement_positions = List.filter
    (fun (p : Lang.Ast.pos_span) -> p.begins.pos_cnum <= span.ends.pos_cnum)
    refinement_positions
  in
  begin match splay_check ~options pgm with
  | Grammar.Answer.Found_error msg ->
    Print.print_splay_error span msg;
    Scheduler.Spawn
      [ { span
        ; task = fun () ->
            begin match splay_check ~options stripped_pgm with
            | Grammar.Answer.Found_error _ -> ()
            | _ -> List.iter Print.print_refinement_warning refinement_positions
            end;
            Scheduler.Done
        }
      ; { span
        ; task = fun () ->
            let a = normal_check ~options pgm in
            Print.print_answer span a;
            begin match a with
            | Grammar.Answer.Found_error _ ->
              List.iter Print.print_clear_refinement_warning refinement_positions;
              Scheduler.Cancel_peers span
            | _ -> Scheduler.Done
            end
        }
      ]
  | answer ->
    Print.print_answer span answer;
    Scheduler.Done
  end

let ceval_many ~(options : Concolic.Options.t) ~refinement_positions pgms stripped_pgms =
  Scheduler.round_robin (
    List.map2 (fun (span, pgm) (_, stripped_pgm) ->
      { Scheduler.span
      ; task = fun () ->
          Print.print_pending span;
          match options.splay with
          | Fallback -> handle_fallback ~options ~refinement_positions span pgm stripped_pgm
          | _ ->
            let a = M.begin_ceval ~print_outcome:false ~options pgm in
            Print.print_answer span a;
            Scheduler.Done
      }
    ) pgms stripped_pgms
  )

let find_baseline_error ~options stmts_with_pos =
  let all_disabled = Stmt_check.disable_all_checks stmts_with_pos in
  let baseline =
    Concolic.Loop.begin_ceval ~print_outcome:false ~options (List.map fst all_disabled)
  in
  match baseline with
  | Grammar.Answer.Found_error _ ->
    let min_pos_span = { Lang.Ast.begins = Lexing.dummy_pos ; ends = Lexing.dummy_pos } in
    Stmt_check.mk_pgms all_disabled ~start_pos:min_pos_span
    |> List.find_map (fun (span, pgm) ->
      match Concolic.Loop.begin_ceval ~print_outcome:false ~options pgm with
      | Grammar.Answer.Exhausted -> None
      | answer -> Some (span, answer))
  | _ -> None

let run_typecheck ~(options : Concolic.Options.t) (packet : Protocol.checker_packet) =
  try
    let stmts_with_pos = Parsing.Parse.Positioned.parse_string packet.full_text in
    let stripped_stmts, refinement_positions = Parsing.Parse.parse_stripped packet.full_text in
    let stmts_to_check, stripped_to_check =
      match find_baseline_error ~options stmts_with_pos with
      | None -> stmts_with_pos, stripped_stmts
      | Some (error_span, a) ->
        (* TODO: extend error message to say statements after this are unreachable *)
        let () = Print.print_answer error_span a in
        fst (Stmt_check.split_on_pos stmts_with_pos error_span),
        fst (Stmt_check.split_on_pos stripped_stmts error_span)
    in
    let check_index = Range_check.compute_check_pos stmts_to_check packet.changes in
    begin match check_index with
    | None -> ()
    | Some start_pos ->
      let pgms = Stmt_check.mk_pgms stmts_to_check ~start_pos in
      let stripped_pgms = Stmt_check.mk_pgms stripped_to_check ~start_pos in
      ceval_many ~options ~refinement_positions pgms stripped_pgms
    end
  with
  | Parsing.Parse.Parse_error (_exn, line, col, tok) ->
    Printf.printf "parse_error:%d:%d:%s\n%!" line col tok
  | exn ->
    Printf.printf "error:%s\n%!" (Printexc.to_string exn)
