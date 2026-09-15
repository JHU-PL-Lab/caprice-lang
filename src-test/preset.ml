
open Ctl_ast
open Variables

let s (Ident.Ident id) = id

(*
  Preset: splaying can quickly prove this well typed, or the program
    has finitely many paths.

  (* TEST
    typing = exhausted;
    speed = fast;
    flags += " -r";
    typecheck;
  *)
    ===
  (* TEST
    include exhaust;
  *)
*)
let exhaust : Ctl_ast.t =
  [ Env_stmt (Assign (speed, s fast))
  ; Env_stmt (Assign (typing, s exhausted))
  ; Env_stmt (Append (flags, " -r"))
  ; Test Typecheck
  ]

(*
  Preset: there is a refutation, i.e. a path that shows the program is ill-typed.
    First tries to type splay, which will go wrong. Then tries to refute without
    type splaying, where an error is then found.

  (* TEST
    typing = ill-typed;
    speed = fast;
    flags += " -r";
    typecheck;
  *)
    ===
  (* TEST
    include refute;
  *)
*)
let refute : Ctl_ast.t =
  [ Env_stmt (Assign (speed, s fast))
  ; Env_stmt (Assign (typing, s ill_typed))
  ; Env_stmt (Append (flags, " -r"))
  ; Test Typecheck
  ]

(*
  Preset: there are infinitely many paths, and splaying is
    incomplete; we are expecting timeout on these tests.

  (* TEST
    typing = no-error;
    speed = slow;
    flags += " -t 3.0 -r";
    typecheck;
  *)
    ===
  (* TEST
    include diverges;
  *)
*)
let diverges : Ctl_ast.t =
  [ Env_stmt (Assign (speed, s slow))
  ; Env_stmt (Assign (typing, s no_error))
  ; Env_stmt (Append (flags, " -t 3.0 -r"))
  ; Test Typecheck
  ]

let lookup : preset -> Ctl_ast.t = function
  | Preset Ident "exhaust" -> exhaust
  | Preset Ident "refute" -> refute
  | Preset Ident "diverges" -> diverges
  | Preset Ident s -> invalid_arg (Printf.sprintf "CTL: unknown preset %s" s)
