(* A quick script to check solver solutions are OK *)
open Smt
open Overlays

module AsciiSymbol = Symbol.AsciiSymbol

let main_solve =
  Solve.main_solve (module Typed_z3.Default)

let key =
  function
  | Model.Bool_key k
  | Model.Int_key k ->
    AsciiSymbol.to_string k

let rec model_assigns_all_symbols
  : type a k. k Model.t -> (a, k) Formula.t -> bool =
  fun model formula ->
  match formula with
  | Formula.Key (Symbol.I uid) ->
    Option.is_some (model.value (Symbol.I uid))

  | Formula.Key (Symbol.B uid) ->
    Option.is_some (model.value (Symbol.B uid))

  | Formula.Const_int _
  | Formula.Const_bool _ ->
    true

  | Formula.Not f ->
    model_assigns_all_symbols model f

  | Formula.And fs ->
    List.for_all (model_assigns_all_symbols model) fs

  | Formula.Binop (_, l, r) ->
    model_assigns_all_symbols model l
    && model_assigns_all_symbols model r

type 'k check_failure =
  | Expected_sat_but_got_unsat of {
      z3_model : 'k Model.t;
    }
  | Expected_unsat_but_got_sat of {
      model : 'k Model.t;
      eval_result : bool;
    }
  | Incomplete_model of {
      model : 'k Model.t;
      z3_result : 'k Solution.t;
    }
  | Inconsistent_model of {
      model : 'k Model.t;
      z3_result : 'k Solution.t;
    }
  | Unexpected_unknown

let solution_kind = function
  | Solution.Sat _ -> "SAT"
  | Solution.Unsat -> "UNSAT"
  | Solution.Unknown -> "UNKNOWN"

let () =
  let fs = Boolean.from_stdin () in

  let check_one i f_text =
    let f =
      Boolean.parse f_text
    in

    let my_result =
      main_solve f
    in

    let z3_result =
      Solve.direct_solve (module Typed_z3.Default) f
    in

    match my_result, z3_result with
    | Solution.Unknown, _ ->
      Some (i + 1, f_text, my_result, z3_result, Unexpected_unknown)

    | Solution.Unsat, Solution.Unsat ->
      None

    | Solution.Unsat, Solution.Unknown ->
      failwith "z3 should not return unknown in sanity_check"

    | Solution.Unsat, Solution.Sat z3_model ->
      Some
        ( i + 1
        , f_text
        , my_result
        , z3_result
        , Expected_sat_but_got_unsat { z3_model }
        )

    | Solution.Sat model, Solution.Unsat ->
      let eval_result =
        Formula.default_eval model f
      in
      Some
        ( i + 1
        , f_text
        , my_result
        , z3_result
        , Expected_unsat_but_got_sat { model; eval_result }
        )

    | Solution.Sat _, Solution.Unknown ->
      failwith "z3 should not return unknown in sanity_check"

    | Solution.Sat model, Solution.Sat _z3_model ->
      let is_full_assignment =
        model_assigns_all_symbols model f
      in

      if not is_full_assignment then
        Some
          ( i + 1
          , f_text
          , my_result
          , z3_result
          , Incomplete_model { model; z3_result }
          )
      else
        let eval_result =
          Formula.default_eval model f
        in

        if eval_result then
          None
        else
          Some
            ( i + 1
            , f_text
            , my_result
            , z3_result
            , Inconsistent_model { model; z3_result }
            )
  in

  let failures =
    List.filter_mapi check_one fs
  in

  let print_failure (i, f_text, my_result, z3_result, failure) =
    Printf.printf "\n--- Invalid formula %d ---\n" i;
    Printf.printf "Formula: %s\n" f_text;
    Printf.printf "Expected from Z3: %s\n" (solution_kind z3_result);
    Printf.printf "Got from main_solve: %s\n" (solution_kind my_result);

    begin
      match failure with
      | Expected_sat_but_got_unsat { z3_model } ->
        Printf.printf "Mismatch: solver returned UNSAT, but Z3 found SAT.\n";
        Printf.printf "Z3 model:\n%s\n" (Model.to_string z3_model ~key)

      | Expected_unsat_but_got_sat { model; eval_result } ->
        Printf.printf "Mismatch: solver returned SAT, but Z3 says UNSAT.\n";
        Printf.printf "Your model evaluates formula to: %b\n" eval_result;
        Printf.printf "Your model:\n%s\n" (Model.to_string model ~key)

      | Incomplete_model { model; z3_result = _ } ->
        Printf.printf "Mismatch: solver returned SAT, but model is incomplete.\n";
        Printf.printf "Your model:\n%s\n" (Model.to_string model ~key)

      | Inconsistent_model { model; z3_result = _ } ->
        Printf.printf
          "Mismatch: solver returned SAT, but model does not satisfy the formula.\n";
        Printf.printf "Formula.default_eval model f = false\n";
        Printf.printf "Your model:\n%s\n" (Model.to_string model ~key)

      | Unexpected_unknown ->
        Printf.printf "Mismatch: solver returned UNKNOWN.\n"
    end
  in

  if List.is_empty failures then
    Printf.printf "checks out!\n"
  else begin
    Printf.printf "Invalid formulas:";
    List.iter
      (fun (i, _, _, _, _) -> Printf.printf " %d," i)
      failures;
    Printf.printf "\n";

    List.iter print_failure failures
  end
