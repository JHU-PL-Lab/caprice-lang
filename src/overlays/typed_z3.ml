
open Smt

module type CONTEXT = sig
  val ctx : Z3.context
end

(*
  Z3 expressions using some context.
*)
module Make_of_context (C : CONTEXT) : Solve.SOLVABLE = struct
  (* I'm relying on internal correctness, and the types are phantom *)
  type ('a, 'k) t = Z3.Expr.expr

  let ctx = C.ctx

  let equal = Z3.Expr.equal

  let const_int (i : int) : (int, 'k) t = Z3.Arithmetic.Integer.mk_numeral_i ctx i
  let const_bool (b : bool) : (bool, 'k) t = Z3.Boolean.mk_val ctx b

  let zero = const_int 0
  let one = const_int 1

  let intS = Z3.Arithmetic.Integer.mk_sort ctx
  let boolS = Z3.Boolean.mk_sort ctx

  let mk_symbol sort uid =
    Z3.Expr.mk_const ctx (Z3.Symbol.mk_int ctx (Utils.Uid.to_int uid)) sort

  let symbol (type a) (s : (a, 'k) Symbol.t) : (a, 'k) t =
    match s with
    | I k -> mk_symbol intS k
    | B k -> mk_symbol boolS k

  let not_ (e : (bool, 'k) t) : (bool, 'k) t =
    Z3.Boolean.mk_not ctx e

  let list_curry f x y = f [ x ; y ]

  let rec binop : type a b c. (a * a * b, c) Binop.c -> (a, 'k) t -> (a, 'k) t -> (b, 'k) t = function
    | Plus            -> list_curry @@ Z3.Arithmetic.mk_add ctx
    | Minus           -> list_curry @@ Z3.Arithmetic.mk_sub ctx
    | Times           -> list_curry @@ Z3.Arithmetic.mk_mul ctx
    | Less_than       -> Z3.Arithmetic.mk_lt ctx
    | Less_than_eq    -> Z3.Arithmetic.mk_le ctx
    | Greater_than    -> Z3.Arithmetic.mk_gt ctx
    | Greater_than_eq -> Z3.Arithmetic.mk_ge ctx
    | Iff
    | Equal           -> Z3.Boolean.mk_eq ctx
    | Not_equal       -> fun a b -> not_ (Z3.Boolean.mk_eq ctx a b)
    | Or              -> list_curry @@ Z3.Boolean.mk_or ctx
    (* OCaml division and modulus differ from Z3, so we need some extra encoding *)
    | Divide -> fun x y ->
      let q0 = Z3.Arithmetic.mk_div ctx x y in
      Z3.Boolean.mk_ite ctx
        (binop Less_than x (binop Times y q0))
        (Z3.Boolean.mk_ite ctx
          (binop Less_than zero y)
          (binop Plus q0 one)
          (binop Minus q0 one))
        q0
    | Modulus -> fun x y ->
      binop Minus x (binop Times y (binop Divide x y))

  let is_const (type a) (x : (a, 'k) t) : bool =
    Z3.Expr.is_const x

  let and_ (exprs : (bool, 'k) t list) : (bool, 'k) t =
    Z3.Boolean.mk_and ctx exprs

  let or_ (exprs: (bool, 'k) t list) : (bool, 'k) t =
    Z3.Boolean.mk_or ctx exprs

  let solver = Z3.Solver.mk_simple_solver ctx

  let set_timeout time =
    time
    |> Utils.Time.span_to_ms
    |> Float.to_int
    |> Int.to_string
    |> Z3.Params.update_param_value ctx "timeout"

  let () = set_timeout (Mtime.Span.(100 * ms))

  let unbox_int_expr e =
    if Z3.Expr.is_numeral e
    then
      Z3.Arithmetic.Integer.get_big_int e
      |> Big_int_Z.int_of_big_int
      |> Option.some
    else None

  let unbox_bool_expr e =
    if Z3.Boolean.is_bool e
    then
      match Z3.Boolean.get_bool_value e with
      | L_FALSE -> Some false
      | L_TRUE -> Some true
      | L_UNDEF -> failwith "Invariant failure: undefined bool."
    else None

  let a_of_expr z3_model expr unbox_expr =
    Option.bind (Z3.Model.get_const_interp_e z3_model expr) unbox_expr

  let solve (e : (bool, 'k) t) : 'k Solution.t =
    if Z3.Expr.equal e (const_bool false) then
      Unsat
    else
      (* Must use the solver stack in order to not keep decls around from previous solves *)
      (* (and this is faster than making a new solver) *)
      let () = Z3.Solver.push solver in
      let result = Z3.Solver.check solver [ e ] in
      let solution =
        match result with
        | Z3.Solver.SATISFIABLE ->
          let model = Option.get @@ Z3.Solver.get_model solver in
          let value : type a. (a, 'k) Symbol.t -> a option = fun s ->
            match s with
            | I _ -> a_of_expr model (symbol s) unbox_int_expr
            | B _ -> a_of_expr model (symbol s) unbox_bool_expr
          in
          let domain =
            Z3.Model.get_const_decls model
            |> List.filter_map (fun decl ->
              let uid =
                decl
                |> Z3.FuncDecl.get_name
                |> Z3.Symbol.get_int
                |> Utils.Uid.of_int
              in

              let range = Z3.FuncDecl.get_range decl in
              let kind = Z3.Sort.get_sort_kind range in
              if kind = Z3enums.BOOL_SORT then
                Some (Model.Bool_key uid)
              else if kind = Z3enums.INT_SORT then
                Some (Model.Int_key uid)
              else
                None
            )
          in
          Solution.Sat { value ; domain }
        | UNKNOWN -> Unknown
        | UNSATISFIABLE -> Unsat
      in
      let () = Z3.Solver.pop solver 1 in
      solution
end

module Make () = Make_of_context (struct let ctx = Z3.mk_context [] end)

module Default = Make ()

include Default
