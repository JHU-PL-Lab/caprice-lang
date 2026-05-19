
module type SOLVABLE = sig
  include Formula.S

  val solve : (bool, 'k) t -> 'k Solution.t
end

type 'k solver = (bool, 'k) Formula.t -> 'k Solution.t
type 'k simplifier = 'k solver -> 'k solver

let direct_solve (module X : SOLVABLE) : 'k solver = fun e ->
  X.solve (Formula.transform (module X) e)

(*
  First attempts to solve with a few heuristics, and then calls the solver.
  This simply special-cases on some common formulas. It also extracts out
  constant assignments (variable = constant).

  As more simplifiers are added, we could instead name this after implied
  concretization.
*)
let rec simplify : 'k simplifier = fun solve expr ->
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
    begin match k, k' with
    | I _, I _ -> Solution.merge (assign 0 k) (assign 0 k')
    | B _, B _ -> Solution.merge (assign true k) (assign true k')
    end
  | Not Binop (Equal, Key k, Key k') ->
    begin match k, k' with
    | I _, I _ -> Solution.merge (assign 0 k) (assign 1 k')
    | B _, B _ -> Solution.merge (assign true k) (assign false k')
    end
  | And e_ls ->
    (*
      If there is any (key = int) formula, then we can subst it through, for it
      is an "implied concretization".

      This idea originates with KLEE (https://dl.acm.org/doi/abs/10.5555/1855741.1855756)
      from Section 3.3, paragraph _Constraint Set Simplification_.
    *)
    let find (e : ('a, 'k) Formula.t) : (const:int * (int, 'k) Symbol.t) option =
      match e with
      | Binop (Equal, Key k, Const_int const) -> Some (~const, k)
      | _ -> None
    in
    begin match List.find_map find e_ls with
    | Some (~const, k) ->
      let reduced_expr = Formula.and_ (List.map (Formula.subst const k) e_ls) in
      Solution.merge (simplify solve reduced_expr) (assign const k)
    | None ->
      solve expr
    end
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
  let solution = simplify (direct_solve (module Oracle)) e in
  let () =
    assert (
      match solution with
      | Solution.Unknown -> true
      | Sat model -> Formula.eval ~default:(fun _ -> assert false) model e
      | Unsat -> Solution.Unsat = direct_solve (module Oracle) e
    )
  in
  solution
