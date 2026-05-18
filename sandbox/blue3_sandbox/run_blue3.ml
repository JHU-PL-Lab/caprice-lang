[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

open Smt
open Sandbox

module AsciiSymbol = Symbol.AsciiSymbol

let sat_formula_to_string (formula : Sat.Formula.formula) : string =
  Format.asprintf
    "%a"
    (Sat.Formula.pp_formula ~uid:AsciiSymbol.to_string)
    formula

let make_bool = AsciiSymbol.make_bool
let make_int = fun x -> x |> AsciiSymbol.make_int |> Formula.symbol

let key =
  function
  | Model.Bool_key k
    | Model.Int_key k ->
    AsciiSymbol.to_string k

let to_string = Formula.to_string ~key

let a = Formula.symbol (AsciiSymbol.make_int 'a')
let b = Formula.symbol (AsciiSymbol.make_int 'b')
let c = Formula.symbol (AsciiSymbol.make_int 'c')
let d = Formula.symbol (AsciiSymbol.make_int 'd')

let solution_text (solution : 'k Solution.t) : string =
  Solution.to_string solution ~key

open Overlays

let main_solve =
  Solve.main_solve (module Typed_z3.Default)

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

let sanity_check () =
  let fs = Boolean.from_stdin () in

  let key = function
    | Model.Bool_key k
      | Model.Int_key k ->
      AsciiSymbol.to_string k
  in

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

let sat_clause_is_2sat (clause : Sat.Formula.clause) : bool =
  List.length clause <= 2

let sat_lit_is_positive (lit : Sat.Formula.literal) : bool =
  match lit with
  | Pos _ -> true
  | Neg _ -> false

let sat_clause_is_horn (clause : Sat.Formula.clause) : bool =
  clause
  |> List.filter sat_lit_is_positive
  |> List.length
  |> fun positive_count -> positive_count <= 1

let sat_formula_is_2sat (sat_formula : Sat.Formula.formula) : bool =
  List.for_all sat_clause_is_2sat sat_formula

let sat_formula_is_horn (sat_formula : Sat.Formula.formula) : bool =
  List.for_all sat_clause_is_horn sat_formula

let print_drop_redundant_ineqs () =
  let fs = Boolean.from_stdin () in

  fs
  |> List.iteri (fun i f_text ->
    let before =
      Boolean.parse f_text
    in

    let after =
      Ints.drop_redundant_ineqs before
    in

    Printf.printf
      "\n--- Formula %d ---\nbefore: %s\nafter:  %s\n"
      (i + 1)
      (Formula.to_string ~key before)
      (Formula.to_string ~key after))

let () = Benchmark.benchmark 2000
