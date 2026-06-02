
type t =
  | Found_error of string   (* found an error *)
  | Timeout of Mtime.Span.t (* global timeout *)
  | Unknown                 (* solver timeout leads to unknown path *)
  | Exhausted_pruned        (* ran all paths up to some depth *)
  | Exhausted               (* completely ran all possible paths *)

let min a b =
  match a, b with
  (* First quickly enumerate the cases where a is strictly smaller *)
  | Exhausted_pruned, Exhausted
  | Unknown, (Exhausted | Exhausted_pruned)
  | Timeout _, (Exhausted | Exhausted_pruned | Unknown)
  | Found_error _, _ -> a
  (* Otherwise b is minimum *)
  | _ -> b

let prune a =
  min a Exhausted_pruned

let to_string = function
  | Found_error msg  -> Printf.sprintf "Found error: %s" msg
  | Timeout span     -> Printf.sprintf "Timeout in %0.3fs" (Utils.Time.convert_span span ~to_:Mtime.Span.s)
  | Unknown          -> "Unknown"
  | Exhausted_pruned -> "Exausted pruned tree"
  | Exhausted        -> "Exhausted"

let is_error = function
  | Found_error _ -> true
  | _ -> false
