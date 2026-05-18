open Formula
open Utils

type reason =
  | Decided
  | Propagated of literal list

let pp_reason ~uid fmt =
  function
  | Decided -> Format.fprintf fmt "Decided"
  | Propagated clause ->
      Format.fprintf fmt "Propagated(%a)" (Formula.pp_clause ~uid) clause

type step = { level : int ; lit : literal ; reason : reason }

type trail = step list

let pp_trail ~uid fmt (trail : trail) : unit =
  let n = List.length trail in
  Format.fprintf fmt "[";
  List.iteri
    (fun i t ->
      Format.fprintf fmt
        "{ \"level\": %d, \"lit\": %a, \"reason\": %a }%s"
    t.level (Formula.pp_literal ~uid) t.lit
    (pp_reason ~uid) t.reason
    (if i < n - 1 then "," else ""))
    trail;
  Format.fprintf fmt "]"

let to_model trail =
  List.map (fun entry -> entry.lit) trail

(** [find_opt lit trail] returns the entry from TRAIL with LIT *)
let find_opt (lit : literal) (trail : step list) : step option =
  List.find_opt (fun entry -> atom_from_literal entry.lit = atom_from_literal lit) trail

(** [find_level lit trail] returns the decision level of LIT in TRAIL or throws if LIT doesn't exist in TRAIL *)
let find_level (lit : literal) (trail : step list) : int =
  match find_opt lit trail with
  | None -> failwith "\n[find_level]: literal is not assigned!"
  | Some entry -> entry.level

(** [find_reason_opt lits trail] returns the reason clause of the
    first propagated literal from LITS in TRAIL, or throws if that doesn't exist *)
let find_reason_opt (lits : literal list) (trail : trail) : clause option =
  List.find_map
    (fun lit ->
      match find_opt lit trail with
      | Some { reason = Propagated reason_clause; _ } -> Some reason_clause
      | _ -> None)
      lits

(** [find_reason lits trail] returns the first literal in LITS that exists in 
    TRAIL *and* has [reason = Propagated] if it exists otherwise it throws *)
let find_reason (lits : literal list) (trail : trail) : clause =
  match find_reason_opt lits trail with
  | Some clause -> clause
  | None ->
    failwith
      (Format.asprintf
        "no propagated literal from LITS in TRAIL\nlits = %a\ntrail = %a"
        (Formula.pp_clause ~uid:(fun uid -> Int.to_string @@ Utils.Uid.to_int uid)) lits
        (pp_trail ~uid:(fun uid -> Int.to_string @@ Utils.Uid.to_int uid)) trail)

let rec analyze_conflict ~clause level trail =
  match List.filter (fun lit -> find_level lit trail = level) clause with
  | [hd] ->
    if level = 0 then [], -1
    else
      let new_lvl =
        clause
        |> List_utils.remove1 hd
        |> List.fold_left
            (fun lvl' lit ->
              max lvl' (find_level lit trail)) 0
      in
      clause, new_lvl
  | current_level_lits ->
    let reason = find_reason current_level_lits trail in
    let clause' = Formula.resolve_pair clause reason in
    analyze_conflict ~clause:clause' level trail

let backjump ~level:backtrack_level trail =
  List.filter (fun { level ; _ } -> level <= backtrack_level) trail

let decide ~lit level trail = { level ; lit ; reason = Decided } :: trail

let imply ~reason level lit trail =
  { level ; lit ; reason = Propagated reason } :: trail
