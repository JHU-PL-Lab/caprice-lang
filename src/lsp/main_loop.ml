module M = Concolic.Loop.Make (Scheduler.Pause_effect)

let splay_check ~options pgm =
  M.begin_ceval ~print_outcome:false ~options:{ options with splay = Splay_only } pgm

let normal_check ~options pgm =
  M.begin_ceval ~print_outcome:false ~options:{ options with splay = Never_splay } pgm

(*
  When splaying has failed, we then race two tasks:
  1. Type splaying without any refinement types because refinement types are the
      most common origin of incompleteness when splaying.
  2. Type check the plain program without splaying.

  If we find a type error, we need to cancel the unrefined splaying attempt, or
  clear its output if it finished first.

  If normal type checking times out, it is useful information to know that the
  refinements are getting in the way of successfully type-splaying the program.
*)
let handle_fallback ~options ~refinement_positions pos pgm unrefined_pgm =
  let refinement_positions =
    List.filter (fun p ->
      p.Utils.Pos.Span.begins.pos_cnum <= pos.Lang.Ast.full.ends.pos_cnum
    ) refinement_positions
  in
  begin match splay_check ~options pgm with
  | Grammar.Answer.Found_error msg ->
    Print.print_splay_error pos.small msg;
    Scheduler.Spawn
      [ { loc = pos.full.begins
        ; task = fun () ->
            begin match splay_check ~options unrefined_pgm with
            | Grammar.Answer.Found_error _ -> ()
            | _ -> List.iter Print.print_refinement_warning refinement_positions
            end;
            Scheduler.Done
        }
      ; { loc = pos.full.begins
        ; task = fun () ->
            let a = normal_check ~options pgm in
            Print.print_answer pos a;
            begin match a with
            | Grammar.Answer.Found_error _ ->
              List.iter Print.print_clear_range refinement_positions;
              Scheduler.Cancel_peers pos.full.begins
            | _ -> Scheduler.Done
            end
        }
      ]
  | answer ->
    Print.print_answer pos answer;
    Scheduler.Done
  end

(*
  Check all of the programs. Depending on options, this first tries to splay,
  and then it falls back to non-splaying and also checking the unrefined
  programs.
*)
let check_all ~(options : Concolic.Options.t) ~refinement_positions pgms unrefined_pgms =
  Scheduler.round_robin (
    List.map2 (fun (pos, pgm) (_, unrefined_pgm) ->
      { Scheduler.loc = pos.Lang.Ast.full.begins
      ; task = fun () ->
          Print.print_pending pos.small;
          match options.splay with
          | Fallback ->
            handle_fallback ~options ~refinement_positions pos pgm unrefined_pgm
          | _ ->
            let a = M.begin_ceval ~print_outcome:false ~options pgm in
            Print.print_answer pos a;
            Scheduler.Done
      }
    ) pgms unrefined_pgms
  )

(*
  Find the first statement in the program that exhibits and error when all
  top-level type annotations are distabled. Once this statement is found, all
  statements after it are void.
*)
let find_baseline_error ~options stmts_with_pos =
  let all_disabled = Stmt_check.disable_all_checks stmts_with_pos in
  let baseline =
    Concolic.Loop.begin_ceval ~print_outcome:false ~options (List.map fst all_disabled)
  in
  match baseline with
  | Grammar.Answer.Found_error _ ->
    let min_pos_span = Utils.Pos.Span.dummy in
    Stmt_check.mk_pgms all_disabled ~start_pos:min_pos_span
    |> List.find_map (fun (span, pgm) ->
      match Concolic.Loop.begin_ceval ~print_outcome:false ~options pgm with
      | Grammar.Answer.Exhausted -> None
      | answer -> Some (span, answer))
  | _ -> None

let run_typecheck ~(options : Concolic.Options.t) (packet : Protocol.checker_packet) =
  try
    let stmts_with_pos = Parsing.Parse.Positioned.parse_string packet.full_text in
    let unrefined_stmts, refinement_positions = Parsing.Parse.parse_unrefined packet.full_text in
    let stmts_to_check, unrefined_to_check =
      match find_baseline_error ~options stmts_with_pos with
      | None -> stmts_with_pos, unrefined_stmts
      | Some (error_span, a) ->
        (* TODO: extend error message to say statements after this are unreachable *)
        let () = Print.print_answer error_span a in
        fst (Stmt_check.split_on_pos stmts_with_pos error_span.full),
        fst (Stmt_check.split_on_pos unrefined_stmts error_span.full)
    in
    let check_index = Range_check.compute_check_pos stmts_to_check packet.changes in
    begin match check_index with
    | None -> ()
    | Some start_pos ->
      let pgms = Stmt_check.mk_pgms stmts_to_check ~start_pos in
      let unrefined_pgms = Stmt_check.mk_pgms unrefined_to_check ~start_pos in
      check_all ~options ~refinement_positions pgms unrefined_pgms
    end
  with
  | Parsing.Parse.Parse_error (_exn, line, col, tok) ->
    Printf.printf "parse_error:%d:%d:%s\n%!" line col tok
  | exn ->
    Printf.printf "error:%s\n%!" (Printexc.to_string exn)
