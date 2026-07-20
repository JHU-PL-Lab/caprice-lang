
(*
  The `Atom_cell` is the payload of int and bool values.
  It is expected to be identity or a pair of concrete and
  symbolic components.
*)
module Make (Atom_cell : Utils.Types.P1) = struct
  type dat = private Data_value
  type typ = private Type_value

  (*
    Data values and type values are all the same type constructor
    so that they are flat, and there is no pointer indirection.
    We can pack them into the same type in an unboxed way. This way,
    the representation is as if they are all just one type.
  *)
  type _ t =
    (* non-type value *)
    | VUnit : dat t
    | VInt : int Atom_cell.t -> dat t
    | VBool : bool Atom_cell.t -> dat t
    | VFunClosure : { param : Ident.t ; closure : Ast.t closure } -> dat t
    | VVariant : any Variant.t -> dat t
    | VRecord : any Record.t -> dat t
    | VModule : any Record.t -> dat t
    | VTuple : any * any -> dat t
    | VFunFix : { fvar : Ident.t ; param : Ident.t ; closure : Ast.t closure } -> dat t
    | VEmptyList : dat t
    | VListCons : { hd : any ; tl : dat t } -> dat t
    (* generated values *)
    | VGenFun : { funtype : (typ t, fun_cod) Funtype.t
                ; table : table Utils.Cell.t option } -> dat t
    | VGenPoly : { id : int ; nonce : int } -> dat t
    | VLazy : lazy_cell -> dat t (* lazily evaluated thing, so state must manage this *)
    (* wrapped values *)
    | VWrapped : { data : dat t ; funtype : (typ t, fun_cod) Funtype.t } -> dat t
    (* type values only *)
    | VType : typ t
    | VTypePoly : { id : int } -> typ t
    | VTypeUnit : typ t
    | VTypeTop : typ t
    | VTypeBottom : typ t
    | VTypeInt : typ t
    | VTypeBool : typ t
    | VTypeMu : { var : Ident.t ; closure : Ast.t closure } -> typ t
    | VTypeList : typ t -> typ t
    | VTypeFun : (typ t, fun_cod) Funtype.t -> typ t
    | VTypeRecord : typ t Record.t -> typ t
    | VTypeModule : (Record.Label.t * Ast.t) list closure -> typ t
    | VTypeVariant : typ t Variant.Label.Map.t -> typ t
    | VTypeRefine : (typ t, Ast.t closure) Refinement.t -> typ t
    | VTypeTuple : typ t * typ t -> typ t
    | VTypeSingle : any -> typ t

  and 'a closure = { captured : 'a ; env : env }

  and table = (any * any) list

  and env = any Env.t

  and fun_cod =
    | CodValue of typ t (* regular function codomain *)
    | CodDependent of Ident.t * Ast.t closure (* dependent function codomain *)

  and any = Any : 'a t -> any [@@unboxed]

  (*
    Represents lazy values. The wrapping types are a queue of types
    with which to wrap the value after it is forced to weak head normal form.

    The lazy state itself is not updated because wrapping is flow sensitive.
  *)
  and lazy_cell = { cell : vlazy Utils.Cell.t ; wrapping_types : typ t list }

  and lgen =
    | LGenList of typ t
    | LGenMu of { var : Ident.t ; closure : Ast.t closure }

  and vlazy =
    | LLazy of lgen
    | LValue of any

  module Env = Env.Make (struct type t = any end)

  type dval = dat t
  type tval = typ t

  let[@inline] to_any : type a. a t -> any = fun v -> Any v

  let[@inline] handle (type a b) (v : a t) ~(dat : dat t -> b) ~(typ : typ t -> b) : b =
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
      | VWrapped _) as x -> dat x
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
      | VTypeSingle _) as x -> typ x

  let[@inline] handle_any (type a) (Any v : any) ~(dat : dat t -> a) ~(typ : typ t -> a) : a =
    handle v ~dat ~typ

  let[@inline] handle_two (v1 : any) (v2 : any)
    (f : [ `Data of dval * dval | `Types of tval * tval | `Mismatch of any * any ] -> 'a) : 'a =
    handle_any v1
      ~dat:(fun d1 ->
        handle_any v2
          ~dat:(fun d2 -> f (`Data (d1, d2)))
          ~typ:(fun _ -> f (`Mismatch (v1, v2)))
        )
      ~typ:(fun t1 ->
        handle_any v2
          ~dat:(fun _ -> f (`Mismatch (v1, v2)))
          ~typ:(fun t2 -> f (`Types (t1, t2)))
      )

  let discard_wrapper : dval -> dval = function
    | VWrapped x -> x.data
    | x -> x

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
      Record.Label.Map.exists (fun _ (Any v') -> contains_mu v') map_body
    | VTuple (Any v1, Any v2) ->
      contains_mu v1 || contains_mu v2
    | VListCons { hd = Any v_hd ; tl } ->
      contains_mu v_hd || contains_mu tl
    | VTypeList t ->
      contains_mu t
    | VTypeRecord record_body ->
      Record.Label.Map.exists (fun _ t -> contains_mu t) record_body
    | VTypeVariant variant_body ->
      Variant.Label.Map.exists (fun _ t -> contains_mu t) variant_body
    | VTypeTuple (t1, t2) ->
      contains_mu t1 || contains_mu t2
    | VTypeSingle Any v ->
      contains_mu v
    | VWrapped { data ; funtype } ->
      contains_mu data || contains_mu (VTypeFun funtype)
    | VTypeFun { domain ; codomain = CodValue t ; mode = _ }
    | VGenFun { funtype = { domain ; codomain = CodValue t ; mode = _ } ; table = _ } ->
      contains_mu domain || contains_mu t
    (* Closures cases: assume true, but may want to inspect closure *)
    | VFunClosure _
    | VFunFix _
    | VTypeModule _
    | VLazy _
    | VGenFun { funtype = { domain = _ ; codomain = CodDependent _ ; mode = _ } ; table = _ }
    | VTypeFun { domain = _ ; codomain = CodDependent _ ; mode = _ } -> true
    (* Refinement types: closure does not escape, so just look at type *)
    | VTypeRefine { tau ; _ } -> contains_mu tau

  let default_constructor (variant_t : tval Variant.Label.Map.t) : Variant.Label.t =
    (* Default is a random variant constructor whose payload does not contain a mu type *)
    let without_mu =
      Variant.Label.Map.filter (fun _ payload ->
          not (contains_mu payload)
        ) variant_t
    in
    match Variant.Label.Map.random_binding_opt without_mu with
    | Some (label, _) -> label
    | None -> fst (Option.get (Variant.Label.Map.random_binding_opt variant_t))

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
      Printf.sprintf "(%s %s)" (Variant.Label.to_string label) (any_to_string payload)
    | VRecord map_body ->
      let fields =
        Record.Label.Map.list_map (fun key data ->
          Printf.sprintf "%s = %s" (Record.Label.to_string key) (any_to_string data)
        ) map_body
      in
      Printf.sprintf "{ %s }" (String.concat " ; " fields)
    | VModule map_body ->
      let decls =
        Record.Label.Map.list_map (fun key data ->
          Printf.sprintf "let %s = %s" (Record.Label.to_string key) (any_to_string data)
        ) map_body
      in
      Printf.sprintf "struct %s end" (String.concat " " decls)
    | VTuple (v1, v2) ->
      Printf.sprintf "(%s, %s)" (any_to_string v1) (any_to_string v2)
    | VFunFix { fvar ; param ; closure = _ } ->
      Printf.sprintf "(fix %s(%s). <body>)" (Ident.to_string fvar) (Ident.to_string param)
    | VEmptyList ->
      "[]"
    | VListCons { hd ; tl } ->
      Printf.sprintf "(%s :: %s)" (any_to_string hd) (to_string tl)
    | VGenFun { funtype ; table = Some table } ->
      Printf.sprintf "G(%s, %d)" (to_string (VTypeFun funtype)) (Utils.Cell.id table)
    | VGenFun { funtype ; table = None } ->
      Printf.sprintf "G(%s)" (to_string (VTypeFun funtype))
    | VGenPoly { id ; nonce } ->
      Printf.sprintf "G(poly id : %d, nonce : %d)" id nonce
    | VWrapped { data ; funtype } ->
      Printf.sprintf "W(%s, %s)" (to_string data) (to_string (VTypeFun funtype))
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
      begin match codomain with
      | CodValue cod_tval ->
        Printf.sprintf "%s %s %s"
          (to_string domain) (Funtype.mode_to_string mode) (to_string cod_tval)
      | CodDependent (id, _closure) ->
        Printf.sprintf "(%s : %s) %s <codomain>"
          (Ident.to_string id) (to_string domain) (Funtype.mode_to_string mode)
      end
    | VTypeRecord map_body ->
      if Record.Label.Map.is_empty map_body then "{:}" else
      let decls =
        Record.Label.Map.list_map (fun label tau ->
          Printf.sprintf "%s : %s" (Record.Label.to_string label) (to_string tau)
        ) map_body
      in
      Printf.sprintf "{ %s }" (String.concat " ; " decls)
    | VTypeModule { captured = table ; env = _ } ->
      let vals =
        List.map (fun (label, _closure) ->
          Printf.sprintf "val %s" (Record.Label.to_string label)
        ) table
      in
      Printf.sprintf "sig %s end" (String.concat " " vals)
    | VTypeVariant map_body ->
      let constructors =
        Variant.Label.Map.list_map (fun label tau ->
          Printf.sprintf "%s of %s" (Variant.Label.to_string label) (to_string tau)
        ) map_body
      in
      Printf.sprintf "(%s)" (String.concat " | " constructors)
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
      let cases = String.concat " | " (List.map Pattern.to_string patterns) in
      Printf.sprintf "Bad match: %s is not in pattern list %s"
        (any_to_string v) cases

    let missing_label (v : any) (label : Record.Label.t) : string =
      Printf.sprintf "Missing label: %s does not have label %s"
        (any_to_string v) (Record.Label.to_string label)

    let project_non_record (v : any) (label : Record.Label.t) : string =
      Printf.sprintf "Bad projection: %s is not a record/module; tried to project label %s"
        (any_to_string v) (Record.Label.to_string label)

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

    let non_type_value (v : dat t) : string =
      Printf.sprintf "Bad type: %s is expected to be a type value"
        (to_string v)

    let non_bool_predicate (v : any) : string =
      Printf.sprintf "Bad predicate: the refinement predicate %s is expected to be a boolean"
        (any_to_string v)

    let wrap_bottom (v : any) : string =
      Printf.sprintf "Bad wrap: tried to wrap %s with type bottom"
        (any_to_string v)

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
    let matches (type a) (pat : Pattern.t) (v : a t)
        ~(resolve_lazy : lazy_cell -> any m) : Match_result.t m =
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
            if Variant.Label.equal pattern_label subject_label
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
            return @@ Match (Env.extend env ~with_:env')
          )
        )
      in
      matches pat v

    let match_any (pat : Pattern.t) (Any v : any)
        ~(resolve_lazy : lazy_cell -> any m) : Match_result.t m =
      matches pat v ~resolve_lazy
  end
end

(*
  Ints and bools have only a concrete component.
  Also there are no lazy values, so they are empty.
*)
module Concrete = Make (Utils.Identity)

include Concrete
