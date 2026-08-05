open Smt

let int_symbol uid =
  Symbol.I (Utils.Uid.of_int uid)

let int_key uid =
  Formula.symbol (int_symbol uid)

let int value =
  Formula.const_int value

let check_formula message expected actual =
  Alcotest.(check bool) message true (Formula.equal expected actual)

let linearize_affine_comparison () =
  let x = int_key 0 in
  let input =
    Formula.binop Less_than_eq
      (Formula.binop Plus x (int 2))
      (int 5)
  in
  let expected = Formula.binop Less_than_eq x (int 3) in
  check_formula "linearized comparison" expected (Ints.linearize input)

let tighten_to_singleton () =
  let x = int_key 0 in
  let input = Formula.and_ [
    Formula.binop Less_than_eq (int 1) x;
    Formula.binop Less_than_eq x (int 3);
    Formula.binop Not_equal x (int 1);
    Formula.binop Not_equal x (int 2);
  ] in
  let expected = Formula.binop Equal x (int 3) in
  check_formula "singleton bound" expected (Ints.tighten_bounds input)

let detect_contradictory_bounds () =
  let x = int_key 0 in
  let input = Formula.and_ [
    Formula.binop Less_than_eq (int 4) x;
    Formula.binop Less_than_eq x (int 3);
  ] in
  check_formula
    "contradictory bounds"
    (Formula.const_bool false)
    (Ints.tighten_bounds input)

let reduce_to_fixed_point () =
  let x_symbol = int_symbol 0 in
  let y_symbol = int_symbol 1 in
  let x = Formula.symbol x_symbol in
  let y = Formula.symbol y_symbol in
  let input = Formula.and_ [
    Formula.binop Equal x (int 2);
    Formula.binop Less_than_eq (Formula.binop Plus x (int 1)) y;
    Formula.binop Less_than_eq y (int 3);
  ] in
  match Simplify.reduce input with
  | Simplify.Contradiction ->
    Alcotest.fail "expected a satisfiable reduction"
  | Simplify.Reduced { residual ; extracted } ->
    check_formula "fully reduced" (Formula.const_bool true) residual;
    Alcotest.(check (option int)) "x assignment" (Some 2)
      (extracted.value x_symbol);
    Alcotest.(check (option int)) "y assignment" (Some 3)
      (extracted.value y_symbol)

let tests =
  ( "SMT simplifiers"
  , [ Alcotest.test_case
        "linearize an affine comparison"
        `Quick
        linearize_affine_comparison
    ; Alcotest.test_case
        "tighten exclusions to a singleton"
        `Quick
        tighten_to_singleton
    ; Alcotest.test_case
        "detect contradictory bounds"
        `Quick
        detect_contradictory_bounds
    ; Alcotest.test_case
        "reduce implied values to a fixed point"
        `Quick
        reduce_to_fixed_point
    ]
  )
