(* Writes out blue3_solve results vs Z3 to stderr
   because the Benchmark library writes to stdout.

   Output is a sqlite table named "benchmarks".
   Usage:
   {[
    dune exec ./benchmarks.exe -- 2000 < formulas.txt \
    2> >(tail -n +3 > benchmarks.sql)
   ]}
*)

open Smt
open Overlays
open Utils

let usage () =
  Printf.eprintf "usage: %s <num_trials>\n" Sys.argv.(0);
  exit 1

let num_trials =
  match Array.to_list Sys.argv with
  | [_program; trials] ->
    begin
      match int_of_string_opt trials with
      | Some n when n > 0 -> n
      | _ -> usage ()
    end
  | _ -> usage ()

let sql_escape (s : string) =
  s
  |> String.split_on_char '\''
  |> String.concat "''"

let blue3_solve expr =
  Solve.main_solve_with_metadata (module Typed_z3.Default) expr

let z3_only_solve =
  Solve.direct_solve (module Typed_z3.Default)

let find_avg label results =
  match List.assoc_opt label results with
  | Some time_us -> time_us
  | None -> failwith ("missing benchmark result: " ^ label)

let run () =
  let fs =
    Boolean.from_stdin ()
  in

  Printf.eprintf
    "CREATE TABLE IF NOT EXISTS benchmarks (\n\
    \  formula_id INTEGER NOT NULL,\n\
    \  formula TEXT NOT NULL,\n\
    \  was_backend_used TEXT NOT NULL,\n\
    \  time_us_blue3 FLOAT NOT NULL,\n\
    \  time_us_z3 FLOAT NOT NULL\n\
     );\n\n";

  fs
  |> List.iteri (fun formula_id ftext ->
    let formula_sql =
      sql_escape ftext
    in

    let f =
      Boolean.parse ftext
    in

    let metadata_ref =
      ref { Solve.was_backend_used = false }
    in

    let results =
      Benchmarker.bench_many_avg
        ~trials:num_trials
        [ ( "blue3"
          , (fun () ->
              let _solution, ~metadata =
                blue3_solve f
              in
              metadata_ref := metadata)
          , ()
          )
        ; ( "z3"
          , (fun () ->
              let f =
                Boolean.parse ftext
              in
              ignore (z3_only_solve f))
          , ()
          )
        ]
    in

    let time_us_blue3 =
      find_avg "blue3" results
    in

    let time_us_z3 =
      find_avg "z3" results
    in

    let was_backend_used =
      !metadata_ref.Solve.was_backend_used
    in

    Printf.eprintf
      "INSERT INTO benchmarks (formula_id, formula, was_backend_used,\
       time_us_blue3, time_us_z3) VALUES (%d, '%s', '%s', %.6f, %.6f);\n"
      formula_id
      formula_sql
      (Bool.to_string was_backend_used)
      time_us_blue3
      time_us_z3)

let () =
  run ()
