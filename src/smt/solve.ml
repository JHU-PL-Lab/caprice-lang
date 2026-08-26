
module type SOLVABLE = sig
  include Formula.S

  val solve : (bool, 'k) t -> 'k Solution.t
end

type 'k solver = (bool, 'k) Formula.t -> 'k Solution.t
type 'k simplifier = 'k solver -> 'k solver

let direct_solve (module X : SOLVABLE) : 'k solver = fun e ->
  X.solve (Formula.transform (module X) e)

(** [solve_trivial solve expr] directly solves a few common formula shapes and
    delegates everything else to [solve]. *)
let solve_trivial : 'k simplifier = fun solve expr ->
  let assign i k = Solution.Sat (Model.singleton i k) in
  (* Hand-write a lot of special cases for single formulas *)
  match expr with
  | Const_bool false -> Unsat
  | Const_bool true -> Sat Model.empty
  | Key k ->
    assign true k
  | Not Key k ->
    assign false k
  | Not (Binop (Equal, Key k, Const_int i)) ->
    assign (if i = 0 then 1 else 0) k
  | Binop ((Equal | Less_than_eq), Key (I _ as k), Const_int i)
  | Binop ((Equal | Less_than_eq), Const_int i, Key (I _ as k)) ->
    assign i k
  | Binop (Less_than, Key k, Const_int i) ->
    assign (i - 1) k
  | Binop (Less_than, Const_int i, Key k) ->
    assign (i + 1) k
  | Binop (Less_than, Key (I _ as k), Key (I _ as k'))
  | Binop (Less_than_eq, Key (I _ as k), Key (I _ as k')) ->
    Solution.merge (assign 0 k) (assign 1 k')
  | Binop (Equal, Key k, Key k') ->
    Solution.merge (assign 0 k) (assign 0 k')
  | Not Binop (Equal, Key k, Key k') ->
    Solution.merge (assign 0 k) (assign 1 k')
  | _ ->
    solve expr

(*
  Simplifies and solves. Asserts correctness of the solution by evaluating
  the expression in the model, or by checking unsatisfiability against the
  oracle, which means the oracle may be used twice for unsat expressions.

  Since assertions are disabled in release mode, this adds no cost to
  benchmarks; it only slows down the test suite.
*)
let main_solve (module Oracle : SOLVABLE) : 'k solver = fun e ->
  let solution =
    match Simplify.reduce e with
    | Simplify.Contradiction -> Solution.Unsat
    | Simplify.Reduced { residual ; extracted } ->
      Solution.merge
        (Solution.Sat extracted)
        (solve_trivial (direct_solve (module Oracle)) residual)
  in
  let () =
    assert (
      match solution with
      | Solution.Unknown -> true
      | Sat model -> Formula.eval ~default:(fun _ -> assert false) model e
      | Unsat -> Solution.Unsat = direct_solve (module Oracle) e
    )
  in
  solution
