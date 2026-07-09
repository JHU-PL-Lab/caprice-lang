
type reason =
  | GenList             (* generate empty or cons *)
  | CheckList           (* check hd or tl *)
  | CheckTuple          (* check left or right side of tuple *)
  | CheckSingleton      (* check subset or superset or intensional equality *)
  | CheckGenFun         (* check domain or codomain *)
  | CheckWrappedFun     (* check domain or codomain of a wrapped function *)
  | CheckRefinementType (* check underlying type or evaluate the predicate *)
  | CheckLetExpr        (* type check a let-expression, or eval body *)
  | ApplGenFun          (* type check argument, or generate result *)
  | ApplWrappedFun      (* type check argument, or evaluate body *)

let reason_to_string = function
  | GenList             -> "Generate list"
  | CheckList           -> "Check list"
  | CheckTuple          -> "Check tuple"
  | CheckSingleton      -> "Check singleton"
  | CheckGenFun         -> "Check generated function"
  | CheckWrappedFun     -> "Check wrapped function"
  | CheckRefinementType -> "Check refinement type"
  | CheckLetExpr        -> "Check let-expression"
  | ApplGenFun          -> "Apply generated function"
  | ApplWrappedFun      -> "Apply wrapped function"

type dir =
  | Gen   (* the label is used to generate something *)
  | Check (* the label is used to check something *)

type t =
  | Left of reason
  | Right of reason
  | Label of Lang.Ident.t * dir

let of_variant_label dir vlabel =
  Label (Lang.Variant.Label.to_ident vlabel, dir)

(* Record labels are generated in product, so there is no branching on them,
  and thus record label tags are always checks. *)
let of_record_label rlabel =
  Label (Lang.Record.Label.to_ident rlabel, Check)

let priority = function
  | Label (_, Gen) -> Priority.one
  | Label (_, Check) -> Priority.zero
  | (Left reason | Right reason) ->
    match reason with
    (* Give priority because we need to stop trying longer lists at some point. *)
    | GenList -> Priority.one
    (* Give no priority for tags that just check without generating more paths.
      If we give priority to these, then we run out of budget very quickly
      because there may be many of these along a single path. *)
    | _ -> Priority.zero

let to_string = function
  | Left reason -> Printf.sprintf "Left (%s)" (reason_to_string reason)
  | Right reason -> Printf.sprintf "Right (%s)" (reason_to_string reason)
  | Label (Ident s, Check) -> Printf.sprintf "%s (Check)" s
  | Label (Ident s, Gen) -> Printf.sprintf "%s (Gen)" s
