
open Ctl_ast
open Variables

type test_expect =
  | Ill_typed (* type refutation found *)
  | Exhausted (* provably well-typed *)
  | No_error  (* no type refutation, and no well-typedness proof *)

let parse_expect = function
  | "ill-typed" -> Ill_typed
  | "no-error" -> No_error
  | "exhausted" | _ -> Exhausted

let parse_speed = function
  | "slow" -> `Slow
  | "fast" | _ -> `Quick

let interp_env (env : Environment.t) (ast : Ctl_ast.t) : Environment.t * testkind =
  let testkind = ref Typecheck in
  let rec interp env ast =
    List.fold_left (fun env -> function
      | Env_stmt Assign (id, s) ->
        Ident.Map.add id s env
      | Env_stmt Append (id, s) ->
        Ident.Map.update id (function
          | Some s' -> Some (s' ^ s)
          | None -> Some s
        ) env
      | Env_stmt Include s ->
        interp env (Preset.lookup s)
      | Test kind ->
        testkind := kind;
        env
    ) env ast
  in
  let e = interp env ast in
  e, !testkind

let get_var env var default =
  Ident.Map.find_opt var env
  |> Option.value ~default

let options_of_env (env : Environment.t) : Concolic.Options.t =
  let flags_str = get_var env flags "" in
  let argv = String.split_on_char ' ' flags_str |> Array.of_list in
  let cmd = Cmdliner.Cmd.v (Cmdliner.Cmd.info "parseflags") Concolic.Options.of_argv in
  match Cmdliner.Cmd.eval_value ~argv cmd with
  | Ok (`Ok options) -> options
  | Ok `Version -> failwith "version requested"
  | Ok `Help -> failwith "help requested"
  | Error _ -> failwith "parse error"

let compute_typecheck_test filename env =
  let expect = parse_expect (get_var env typing exhausted_s) in
  let options = options_of_env env in
  let pgm = Parsing.Parse.parse_file filename in
  let answer = Concolic.Loop.begin_ceval pgm ~options in
  match expect, answer with
  | Ill_typed, Grammar.Answer.Found_error _
  | Exhausted, Exhausted
  | No_error, (Unknown | Exhausted_pruned | Timeout _) -> true
  | _ -> false

let positions_test filename env =
  let expected = Position_checks.parse_positions (get_var env positions "") in
  let actual =
    filename
    |> Parsing.Parse.Positioned.parse_file
    |> List.map (fun (_statement, { Lang.Ast.begins ; ends }) ->
        (Lsp.Positions.of_lexing begins, Lsp.Positions.of_lexing ends))
  in
  expected = actual

let statement_index_test filename env =
  let open Position_checks in
  let stmts_with_pos = Parsing.Parse.Positioned.parse_file filename in
  let span_to_idx span =
    stmts_with_pos
    |> List.find_mapi (fun i (_, s) ->
      if Lang.Ast.equal_pos_span s span then Some i else None)
    |> Option.value ~default:(-1)
  in
  let expected = parse_int_list (get_var env statement_indexes "") in
  let actual =
    parse_changes (get_var env changes "")
    |> List.map (fun change ->
      Lsp.Range_check.compute_check_pos stmts_with_pos [change]
      |> Option.map span_to_idx
      |> Option.value ~default:(-1))
  in
  expected = actual

let check_true msg b =
  Alcotest.(check bool msg true b)

let make_test (filename : string) : unit Alcotest.test_case option =
  match Parse_ctl.parse_test_header filename with
  | None -> None
  | Some ctl_script ->
    let env, testkind = interp_env Environment.default ctl_script in
    let speed_level = parse_speed (get_var env speed fast_s) in
    Option.some @@
    Alcotest.test_case filename speed_level (fun () ->
      match testkind with
      | Skip ->
        Alcotest.skip ()
      | Typecheck ->
        check_true "failed type check" @@
        compute_typecheck_test filename env
      | Position_check ->
        check_true "failed position check" @@
        positions_test filename env
      | Statement_index_check ->
        check_true "failed statement index check" @@
        statement_index_test filename env
    )
