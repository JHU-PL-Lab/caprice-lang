
(*
  The `Atom_cell` is the payload of int and bool values.
  It is expected to be identity or a pair of concrete and
  symbolic components.
*)
module Make (Atom_cell : Utils.Types.P1) = struct
  type data = private Data
  type typeval = private Typeval

  (*
    Data values and type values are all the same type constructor
    so that they are flat, and there is no pointer indirection.
    We can pack them into the same type in an unboxed way. This way,
    the representation is as if they are all just one type.
  *)
  type _ t =
    (* non-type value *)
    | VUnit : data t
    | VInt : int Atom_cell.t -> data t
    | VBool : bool Atom_cell.t -> data t
    | VFunClosure : { param : Ident.t ; closure : Ast.t closure } -> data t
    | VVariant : any Variant.t -> data t
    | VRecord : any Record.t -> data t
    | VModule : any Record.t -> data t
    | VTuple : any * any -> data t
    | VFunFix : { fvar : Ident.t ; param : Ident.t ; closure : Ast.t closure } -> data t (* no mutual recursion yet *)
    | VEmptyList : data t
    | VListCons : { hd : any ; tl : data t } -> data t
    (* generated values *)
    | VGenFun : { funtype : (typeval t, fun_cod) Funtype.t ; nonce : int ; alist : alist Suspension.t option } -> data t
    | VGenPoly : { id : int ; nonce : int } -> data t
    | VLazy : lazy_cell -> data t (* lazily evaluated thing, so state must manage this *)
    (* wrapped values *)
    | VWrapped : { data : data t ; tau : (typeval t, fun_cod) Funtype.t }  -> data t
    (* type values only *)
    | VType : typeval t
    | VTypePoly : { id : int } -> typeval t
    | VTypeUnit : typeval t
    | VTypeTop : typeval t
    | VTypeBottom : typeval t
    | VTypeInt : typeval t
    | VTypeBool : typeval t
    | VTypeMu : { var : Ident.t ; closure : Ast.t closure } -> typeval t
    | VTypeList : typeval t -> typeval t
    | VTypeFun : (typeval t, fun_cod) Funtype.t -> typeval t
    | VTypeRecord : typeval t Record.t -> typeval t
    | VTypeModule : (Labels.Record.t * Ast.t) list closure -> typeval t (* not eagerly evaluating first label *)
    | VTypeVariant : typeval t Labels.Variant.Map.t -> typeval t
    | VTypeRefine : (typeval t, Ast.t closure) Refinement.t -> typeval t
    | VTypeTuple : typeval t * typeval t -> typeval t
    | VTypeSingle : any -> typeval t

  and 'a closure = { captured : 'a ; env : env }

  and alist = (any * any) list

  and env = any Env.t

  and fun_cod =
    | CodValue of typeval t (* regular function codomain *)
    | CodDependent of Ident.t * Ast.t closure (* dependent function codomain *)

  and any = Any : 'a t -> any [@@unboxed]

  (*
    Represents lazy values. The wrapping types are a queue of types
    with which to wrap the value after it is forced to weak head normal form.

    The lazy state itself is not updated because wrapping is flow sensitive.
  *)
  and lazy_cell = { cell : vlazy Suspension.t ; wrapping_types : typeval t list }

  and lgen =
    | LGenList of typeval t
    | LGenMu of { var : Ident.t ; closure : Ast.t closure }

  and vlazy =
    | LLazy of lgen
    | LValue of any

  module Env = Env.Make (struct type t = any end)

  type dval = data t
  type tval = typeval t

  let[@inline] to_any : type a. a t -> any = fun v -> Any v

  let[@inline] handle (type a b) (v : a t) ~(data : data t -> b) ~(typeval : typeval t -> b) : b =
    match v with
    | ( VUnit
      | VInt _
      | VBool _
      | VFunClosure _
      | VVariant _
      | VRecord _
      | VModule _
      | VTuple _
      | VFunFix _
      | VEmptyList
      | VListCons _
      | VGenFun _
      | VGenPoly _
      | VLazy _
      | VWrapped _) as x -> data x
    | ( VType
      | VTypePoly _
      | VTypeUnit
      | VTypeTop
      | VTypeBottom
      | VTypeInt
      | VTypeBool
      | VTypeMu _
      | VTypeList _
      | VTypeFun _
      | VTypeRecord _
      | VTypeModule _
      | VTypeVariant _
      | VTypeRefine _
      | VTypeTuple _
      | VTypeSingle _) as x -> typeval x

  let[@inline] handle_any (type a) (Any v : any) ~(data : data t -> a) ~(typeval : typeval t -> a) : a =
    handle v ~data ~typeval

  let[@inline] handle_two (v1 : any) (v2 : any)
    (f : [ `Data of dval * dval | `Types of tval * tval | `Mismatch of any * any ] -> 'a) : 'a =
    handle_any v1
      ~data:(fun d1 ->
        handle_any v2
          ~data:(fun d2 -> f (`Data (d1, d2)))
          ~typeval:(fun _ -> f (`Mismatch (v1, v2)))
        )
      ~typeval:(fun t1 ->
        handle_any v2
          ~data:(fun _ -> f (`Mismatch (v1, v2)))
          ~typeval:(fun t2 -> f (`Types (t1, t2)))
      )

  (*
    True if the value has any mu type in its representation.
    This is used to dodge recursion by default.
  *)
  let rec contains_mu : type a. a t -> bool = fun v ->
    match v with
    | VUnit
    | VInt _
    | VBool _
    | VGenPoly _
    | VEmptyList
    | VType
    | VTypePoly _
    | VTypeUnit
    | VTypeTop
    | VTypeBottom
    | VTypeInt
    | VTypeBool -> false
    | VTypeMu _ -> true
    (* Recursive cases: contains mu if any of the subvalues does *)
    | VVariant { payload = Any v' ; label = _ } -> contains_mu v'
    | VModule map_body
    | VRecord map_body ->
      Labels.Record.Map.exists (fun _ (Any v') -> contains_mu v') map_body
    | VTuple (Any v1, Any v2) ->
      contains_mu v1 || contains_mu v2
    | VListCons { hd = Any v_hd ; tl } ->
      contains_mu v_hd || contains_mu tl
    | VTypeList t ->
      contains_mu t
    | VTypeRecord record_body ->
      Labels.Record.Map.exists (fun _ t -> contains_mu t) record_body
    | VTypeVariant variant_body ->
      Labels.Variant.Map.exists (fun _ t -> contains_mu t) variant_body
    | VTypeTuple (t1, t2) ->
      contains_mu t1 || contains_mu t2
    | VTypeSingle Any v ->
      contains_mu v
    | VWrapped { data ; tau } ->
      contains_mu data || contains_mu (VTypeFun tau)
    | VTypeFun { domain ; codomain = CodValue t ; mode = _ }
    | VGenFun { funtype = { domain ; codomain = CodValue t ; mode = _  } ; nonce = _ ; alist = _ }->
      contains_mu domain || contains_mu t
    (* Closures cases: assume true, but may want to inspect closure *)
    | VFunClosure _
    | VFunFix _
    | VTypeModule _
    | VLazy _
    | VGenFun { funtype = { domain = _ ; codomain = CodDependent _ ; mode = _ } ; nonce = _ ; alist = _ }
    | VTypeFun { domain = _ ; codomain = CodDependent _ ; mode = _ } -> true
    (* Refinement types: closure does not escape, so just look at type *)
    | VTypeRefine { tau ; _ } -> contains_mu tau

  let default_constructor (variant_t : tval Labels.Variant.Map.t) : Labels.Variant.t =
    (* Default is a random variant constructor whose payload does not contain a mu type *)
    let without_mu =
      Labels.Variant.Map.filter (fun _ payload ->
          not (contains_mu payload)
        ) variant_t
    in
    Labels.Variant.Map.random_binding_opt
      (if Labels.Variant.Map.is_empty without_mu then variant_t else without_mu)
    |> Option.get
    |> fst

  let rec to_string : type a. a t -> string = function
    | VUnit ->
      "()"
    | VInt i ->
      Atom_cell.to_string Int.to_string i
    | VBool b ->
      Atom_cell.to_string Bool.to_string b
    | VFunClosure { param ; closure = _ } ->
      Printf.sprintf "(fun %s -> <body>)" (Ident.to_string param)
    | VVariant { label ; payload } ->
      Printf.sprintf "(%s %s)" (Labels.Variant.to_string label) (any_to_string payload)
    | VRecord map_body ->
      Labels.Record.Map.to_list map_body
      |> List.map (fun (key, data) -> Printf.sprintf "%s = %s" (Labels.Record.to_string key) (any_to_string data))
      |> String.concat " ; "
      |> Printf.sprintf "{ %s }"
    | VModule map_body ->
      Labels.Record.Map.to_list map_body
      |> List.map (fun (key, data) -> Printf.sprintf "let %s = %s" (Labels.Record.to_string key) (any_to_string data))
      |> String.concat " "
      |> Printf.sprintf "struct %s end"
    | VTuple (v1, v2) ->
      Printf.sprintf "(%s, %s)" (any_to_string v1) (any_to_string v2)
    | VFunFix { fvar ; param ; closure = _ } ->
      Printf.sprintf "(fix %s(%s). <body>)" (Ident.to_string fvar) (Ident.to_string param)
    | VEmptyList ->
      "[]"
    | VListCons { hd ; tl } ->
      Printf.sprintf "(%s :: %s)" (any_to_string hd) (to_string tl)
    | VGenFun { funtype ; nonce ; alist = _ } ->
      Printf.sprintf "G(%s, %d)" (to_string (VTypeFun funtype)) nonce
    | VGenPoly { id ; nonce } ->
      Printf.sprintf "G(poly id : %d, nonce : %d)" id nonce
    | VWrapped { data ; tau } ->
      Printf.sprintf "W(%s, %s)" (to_string data) (to_string (VTypeFun tau))
    | VLazy { cell = _ ; wrapping_types } ->
      List.fold_right (fun t acc ->
        Printf.sprintf "W(%s, %s)" acc (to_string t)
      ) wrapping_types "<lazy>"
    | VType ->
      "type"
    | VTypePoly { id } ->
      Printf.sprintf "(poly id : %d)" id
    | VTypeUnit ->
      "unit"
    | VTypeTop ->
      "top"
    | VTypeBottom ->
      "bottom"
    | VTypeInt ->
      "int"
    | VTypeBool ->
      "bool"
    | VTypeMu { var ; closure = _ } ->
      Printf.sprintf "(mu %s. <body>)" (Ident.to_string var)
    | VTypeList t ->
      Printf.sprintf "(list %s)" (to_string t)
    | VTypeFun { domain ; codomain ; mode } ->
      let s_arrow = match mode with Funtype.Nondet -> "->" | Det -> "-->" in
      begin match codomain with
      | CodValue cod_tval -> Printf.sprintf "%s %s %s" (to_string domain) s_arrow (to_string cod_tval)
      | CodDependent (id, _closure) -> Printf.sprintf "(%s : %s) %s <codomain>" (Ident.to_string id) (to_string domain) s_arrow
      end
    | VTypeRecord map_body ->
      if Labels.Record.Map.is_empty map_body then "{:}" else
      Labels.Record.Map.to_list map_body
      |> List.map (fun (label, tau) -> Printf.sprintf "%s : %s" (Labels.Record.to_string label) (to_string tau))
      |> String.concat " ; "
      |> Printf.sprintf "{ %s }"
    | VTypeModule { captured = alist ; env = _ } ->
      alist
      |> List.map (fun (label, _closure) -> Printf.sprintf "val %s" (Labels.Record.to_string label))
      |> String.concat " "
      |> Printf.sprintf "sig %s end"
    | VTypeVariant map_body ->
      Labels.Variant.Map.to_list map_body
      |> List.map (fun (label, tau) -> Printf.sprintf "%s of %s" (Labels.Variant.to_string label) (to_string tau))
      |> String.concat " | "
      |> Printf.sprintf "(%s)"
    | VTypeRefine { var ; tau ; predicate = _closure } ->
      Printf.sprintf "{ %s : %s | <predicate> }" (Ident.to_string var) (to_string tau)
    | VTypeTuple (t1, t2) ->
      Printf.sprintf "(%s * %s)" (to_string t1) (to_string t2)
    | VTypeSingle Any v ->
      Printf.sprintf "(singleton %s)" (to_string v)

  and any_to_string (Any any) = to_string any

  module Error_messages = struct
    let refutation (v : any) (t : tval) : string =
      Printf.sprintf "Refutation: %s does not have type %s"
        (any_to_string v) (to_string t)

    let bad_binop (v1 : any) (op : Binop.t) (v2 : any) : string =
      Printf.sprintf "Bad binop: %s %s %s"
        (any_to_string v1) (Binop.to_string op) (any_to_string v2)

    let apply_non_function (v : any) : string =
      Printf.sprintf "Bad application: %s is not a function"
        (any_to_string v)

    let missing_pattern (v : any) (patterns : Pattern.t list) : string =
      List.map Pattern.to_string patterns
      |> String.concat " | "
      |> Printf.sprintf "Bad match: %s is not in pattern list %s" (any_to_string v)

    let missing_label (v : any) (label : Labels.Record.t) : string =
      Printf.sprintf "Missing label: %s does not have label %s"
        (any_to_string v) (Labels.Record.to_string label)

    let project_non_record (v : any) (label : Labels.Record.t) : string =
      Printf.sprintf "Bad projection: %s is not a record/module; tried to project label %s"
        (any_to_string v) (Labels.Record.to_string label)

    let cons_non_list (v_hd : any) (v_tl : any) : string =
      Printf.sprintf "Bad cons: tried to put %s on front of %s, which is not a list"
        (any_to_string v_hd) (any_to_string v_tl)

    let not_non_bool (v : any) : string =
      Printf.sprintf "Bad not: %s is not a boolean and cannot be negated"
        (any_to_string v)

    let if_non_bool (v : any) : string =
      Printf.sprintf "Bad if: %s is not a boolean and cannot be used as a condition"
        (any_to_string v)

    let assert_non_bool (v : any) : string =
      Printf.sprintf "Bad assert: %s is not a boolean and cannot be used for an assertion"
        (any_to_string v)

    let assume_non_bool (v : any) : string =
      Printf.sprintf "Bad assume: %s is not a boolean and cannot be used for an assumption"
        (any_to_string v)

    let non_type_value (v : data t) : string =
      Printf.sprintf "Bad type: %s is expected to be a type value"
        (to_string v)

    let non_bool_predicate (v : any) : string =
      Printf.sprintf "Bad predicate: the refinement predicate %s is expected to be a boolean"
        (any_to_string v)

    let bad_wrap s v t =
      Printf.sprintf "Bad wrap: %s is not %s; tried to wrap with type %s"
        s (any_to_string v) (to_string t)

    let wrap_bottom (v : any) : string =
      Printf.sprintf "Bad wrap: tried to wrap %s with type bottom"
        (any_to_string v)

    let wrap_non_list (v : any) (tlist : tval) : string =
      bad_wrap "a list" v tlist

    let wrap_typeval_fun (t : tval) (tfun : tval) : string =
      bad_wrap "a function" (Any t) tfun

    let wrap_missing_label (v : any) (label : Labels.Record.t) : string =
      Printf.sprintf "Bad wrap: Missing label: %s does not have label %s"
        (any_to_string v) (Labels.Record.to_string label)

    let wrap_non_record (v : any) (t : tval) : string =
      bad_wrap "a record" v t

    let wrap_non_module (v : any) (t : tval) : string =
      bad_wrap "a module" v t

    let wrap_missing_constructor (v : any) (t : tval) : string =
      Printf.sprintf "Bad wrap: Missing constructor: %s is not a constructor in type %s"
        (any_to_string v) (to_string t)

    let wrap_non_variant (v : any) (t : tval) : string =
      bad_wrap "a variant" v t

    let wrap_non_tuple (v : any) (t : tval) : string =
      bad_wrap "a tuple" v t

    let shape_mismatch (v1 : any) (v2 : any) : string =
      Printf.sprintf "Bad intensional equality: %s and %s are not of the same shape."
        (any_to_string v1) (any_to_string v2)

    let non_contractive_type (t : tval) : string =
      Printf.sprintf "Bad type: %s is not contractive."
        (to_string t)

    let splayed_rec_fun (v_func : dval) (v_arg : any) : string =
      Printf.sprintf "Called rec fun %s with symbolic value %s while splaying"
        (to_string v_func) (any_to_string v_arg)
  end

  module Match_result = struct
    type t =
      | Match of env
      | No_match
      | Failure of string

    let match_ = Match Env.empty
  end

  module Make_match (Monad : Utils.Types.MONAD) = struct
    open Monad

    let ( let* ) = Monad.bind

    let bind_res (m : Match_result.t m) (f : Env.t -> Match_result.t m) : Match_result.t m =
      let* m in
      match m with
      | (No_match | Failure _) as r -> return r
      | Match env -> f env

    (*
      In case we match on a symbol, we must resolve the symbol to a value.
      It's expected that this computation is monadic, so we must pass in
      the monad via a functor.
    *)
    let matches (type a) (pat : Pattern.t) (v : a t) ~(resolve_lazy : lazy_cell -> any m) : Match_result.t m =
      let rec matches
        : type a. Pattern.t -> a t -> Match_result.t m
        = fun p v ->
        let open Match_result in
        match p, v with
        | PAny, _ ->
          return match_
        | PVariable id, v ->
          return @@ Match (Env.singleton id (Any v))
        | PPatternAs (pat, id), v ->
          bind_res (matches pat v) (fun env ->
            return @@ Match (Env.set id (Any v) env)
          )
        | PPatternOr p_ls, v ->
          let rec try_patterns = function
            | [] -> return No_match
            | pat :: rest ->
              let* result = matches pat v in
              match result with
              | No_match -> try_patterns rest
              | _ -> return result
          in
          try_patterns p_ls
        | _, VLazy vlazy ->
          let* (Any v) = resolve_lazy vlazy in
          matches p v
        | p, VGenPoly _ ->
          (* generated polymorphic values cannot be inspected *)
          return @@ Failure
            (Printf.sprintf "Bad match: matching polymorphic value with pattern %s"
              (Pattern.to_string p))
        | PVariant { label = pattern_label ; payload = payload_pattern },
          VVariant { label = subject_label ; payload = Any v } ->
            if Labels.Variant.equal pattern_label subject_label
            then matches payload_pattern v
            else return No_match
        | PTuple (p1, p2), VTuple (Any v1, Any v2) ->
          match_two (p1, v1) (p2, v2)
        | PUnit, VUnit ->
          return match_
        | PEmptyList, VEmptyList ->
          return match_
        | PDestructList (p1, p2), VListCons { hd = Any v1 ; tl = v2 } ->
          match_two (p1, v1) (p2, v2)
        | _ ->
          return No_match

      and match_two
        : type a b. Pattern.t * a t -> Pattern.t * b t -> Match_result.t m
        = fun (p1, v1) (p2, v2) ->
        let open Match_result in
        bind_res (matches p1 v1) (fun env ->
          bind_res (matches p2 v2) (fun env' ->
            return @@ Match (Env.extend env env')
          )
        )
      in
      matches pat v

    let match_any (pat : Pattern.t) (Any v : any) ~(resolve_lazy : lazy_cell -> any m) : Match_result.t m =
      matches pat v ~resolve_lazy
  end
end

(*
  Ints and bools have only a concrete component.
  Also there are no lazy values, so they are empty.
*)
module Concrete = Make (Utils.Identity)

include Concrete
