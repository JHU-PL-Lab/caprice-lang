
type t =
  | PAny
  | PVariable of Ident.t
  | PVariant of t Variant.t (* payload is a pattern *)
  | PTuple of t * t
  | PUnit
  | PEmptyList
  | PDestructList of t * t
  | PPatternOr of t list
  | PPatternAs of t * Ident.t

let rec to_string (p : t) : string =
  match p with
  | PAny -> "_"
  | PVariable id -> Ident.to_string id
  | PVariant { label ; payload } ->
    Printf.sprintf "%s %s" (Variant.Label.to_string label) (to_string payload)
  | PTuple (p1, p2) ->
    Printf.sprintf "(%s, %s)" (to_string p1) (to_string p2)
  | PUnit ->
    "()"
  | PEmptyList ->
    "[]"
  | PDestructList (p_hd, p_tl) ->
    Printf.sprintf "(%s :: %s)" (to_string p_hd) (to_string p_tl)
  | PPatternOr p_ls ->
    Printf.sprintf "(%s)" (String.concat " | " @@ List.map to_string p_ls)
  | PPatternAs (pat, id) ->
    Printf.sprintf "(%s as %s)" (to_string pat) (Ident.to_string id)
