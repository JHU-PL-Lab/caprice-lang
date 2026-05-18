type 'k solver = (bool, 'k) Formula.t -> 'k Solution.t

type 'k simplifier = 'k solver -> 'k solver

(** Add whatever here it doesn't affect the solving *)
type metadata =
  { was_backend_used : bool }

type 'k solver_meta =
  (bool, 'k) Formula.t -> 'k Solution.t * metadata:metadata

type 'k simplifier_meta =
  'k solver_meta -> 'k solver_meta

module type SOLVABLE = sig
  include Formula.S

  val solve : (bool, 'k) t -> 'k Solution.t
end

let direct_solve (module X : SOLVABLE) : 'k solver = fun e ->
  X.solve (Formula.transform (module X) e)

let rec collect_concrete
    (acc : Model.ValueMap.t)
    (f : (bool, 'k) Formula.t)
  : Model.ValueMap.t option =
  match f with
  | Binop (Equal, Key (I k), Const_int i)
  | Binop (Equal, Const_int i, Key (I k)) ->
      Model.ValueMap.add_int_checked k i acc
  | Binop (Equal, Key (B k), Const_bool b)
  | Binop (Equal, Const_bool b, Key (B k)) ->
      Model.ValueMap.add_bool_checked k b acc
  | Key (B k) ->
      Model.ValueMap.add_bool_checked k true acc
  | Not (Key (B k)) ->
      Model.ValueMap.add_bool_checked k false acc
  | And ls ->
      List.fold_left
        (fun acc_opt f ->
          match acc_opt with
          | None -> None
          | Some acc -> collect_concrete acc f)
        (Some acc)
        ls
  | _ -> Some acc

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

(** [implied_concretization next expr] first attempts to solve EXPR with a
    few heuristics, and then calls the solver NEXT.

    This simply special-cases on some common formulas. It also extracts out
    constant assignments (variable = constant).

    As more simplifiers are added, we could instead name this after implied
    concretization. *)
let implied_concretization : 'k simplifier = fun next expr ->
  let open Utils in
  let module ValueMap = Model.ValueMap in
  match collect_concrete Uid.Map.empty expr with
  | None -> Solution.Unsat (* because a variable can't be equal to more than 2 *)
  | Some value_map ->
    let rec sub_concrete_key : type a. (a, 'k) Formula.t  -> (a, 'k) Formula.t =
      fun f ->
      match f with
      | Key (I key) ->
        begin match ValueMap.find_int_opt key value_map with
        | Some iv -> Formula.const_int iv
        | None -> Formula.symbol (I key)
        end
      | Key (B key) ->
        begin
        match ValueMap.find_bool_opt key value_map with
        | Some iv -> Formula.const_bool iv
        | None -> Formula.symbol (B key)
        end
      | Binop (Less_than, left, right) ->
        Formula.binop Binop.Less_than
          (sub_concrete_key left)
          (sub_concrete_key right)
      | Binop (Less_than_eq, left, right) ->
          Formula.binop Binop.Less_than_eq
            (sub_concrete_key left)
            (sub_concrete_key right)
      | Binop (Plus, left, right) ->
          Formula.binop Binop.Plus
            (sub_concrete_key left)
            (sub_concrete_key right)
      | Binop (Minus, left, right) ->
          Formula.binop Binop.Minus
            (sub_concrete_key left)
            (sub_concrete_key right)
      | Binop (Times, left, right) ->
          Formula.binop Binop.Times
            (sub_concrete_key left)
            (sub_concrete_key right)
      | Binop (Divide, left, right) ->
          Formula.binop Binop.Divide
            (sub_concrete_key left)
            (sub_concrete_key right)
      | Binop (Modulus, left, right) ->
          Formula.binop Binop.Modulus
            (sub_concrete_key left)
            (sub_concrete_key right)
      | Binop (Equal, left, right) ->
          Formula.binop Binop.Equal
            (sub_concrete_key left)
            (sub_concrete_key right)
      | Not f -> Formula.not_ (sub_concrete_key f)
      | And ls -> Formula.and_ @@ List.map sub_concrete_key ls
      | f -> f
    in
    let simplified = sub_concrete_key expr in
    match simplified with
    | Const_bool true -> Solution.Sat (Model.from_value_map value_map)
    | Const_bool false -> Solution.Unsat
    | _ ->
    next simplified

let linearize next expr =
  next (Ints.linearize expr)

let drop_redundant_ineqs next expr =
  next (Ints.drop_redundant_ineqs expr)

let add_metadata (simplifier : 'k simplifier)
  : 'k simplifier_meta =
  fun next formula ->
    let metadata = ref { was_backend_used = false } in

    let next_plain formula =
      let solution, ~metadata:metadata' = next formula in
      metadata := metadata';
      solution
    in

    let solution =
      simplifier next_plain formula
    in

    solution, ~metadata:!metadata

(*
  Right-associative simplifier composition.
  E.g. this simplifier
    simpl1 @> simpl2 @> simpl3
  is equivalent to
    simpl1 @> (simpl2 @> simpl3)
  which we can see does simpl1 as a pre-pass to the composition of simpl2
  and simpl3. Hence when given a solver, this first simplifies with simpl1,
  then with simpl2, and finally with simpl3 before calling the solver.
*)
let ( @> ) : 'k simplifier -> 'k simplifier -> 'k simplifier =
  fun f g -> fun solve -> f (g solve)

let ( @@> )
  : 'k simplifier_meta ->
    'k simplifier_meta ->
    'k simplifier_meta =
  fun f g -> fun solve -> f (g solve)

let main_solve (module Oracle : SOLVABLE) : 'k solver =
  let pipeline =
    linearize
    @> implied_concretization
    @> drop_redundant_ineqs
    @> Blue3.blue3 ~solver:Idl.solve_int_diff
  in
  pipeline (direct_solve (module Oracle))

let main_solve_with_metadata (module Oracle : SOLVABLE)
  : 'k solver_meta =
  let pipeline =
    add_metadata linearize
    @@> add_metadata implied_concretization
    @@> add_metadata drop_redundant_ineqs
    @@> add_metadata (Blue3.blue3 ~solver:Idl.solve_int_diff)
  in

  pipeline
    (fun formula ->
       direct_solve (module Oracle) formula,
       ~metadata:{ was_backend_used = true })
