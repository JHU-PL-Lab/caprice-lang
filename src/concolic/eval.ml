
open Lang
open Semantics
open Grammar
open Grammar.Val
open Eval_result

(* `Any` is unboxed, so this is zero overhead *)
let[@inline] return_any v = return (Any v)

let bad_input_env =
  InvariantException "Input environment is ill-formed"

open Grammar.Val.Error_messages

(**
  [eval] returns the list of runs (evaluation) of the program.
  There are multiple runs because it sometimes forks the state
  to symbolically evaluate.
  Every fork calls [Utils.Time.yield_to_timer] so that timeout can be
  noticed reasonably frequently. Hence this must be run within a handler.
*)
let eval
  (pgm : Ast.statement list)
  (input_env : Input_env.t)
  (target : Target.t)
  ~(max_step : Grammar.Step.t)
  ~(default_int : unit -> int)
  ~(default_bool : unit -> bool)
  ~(do_splay : bool)
  ~(do_wrap : bool)
  : Logged_run.t list
  =
  (*
    Reads a tag from the input environment. If the tag was planned,
    then run the left or right accordingly (pushing the tag to this
    path).
    Otherwise, fork on the left and continue on the right.
  *)
  let fork_on_left (type a env) ~(left : 'a. ('a, env) m) ~(right : (a, env) m) ~reason =
    let* () = incr_step ~max_step in
    let run_left =
      let* () = push_and_log_tag @@ Left reason in
      left
    in
    let run_right =
      let* () = push_and_log_tag @@ Right reason in
      right
    in
    let* l_opt = read_input KTag input_env in
    match l_opt with
    | Some Left reason' when reason = reason' -> run_left
    | Some Right reason' when reason = reason' -> run_right
    | Some _ -> raise bad_input_env
    | None -> let* () = fork run_left in run_right
  in

  (*
    ----------------------------
    EVALUATE EXPRESSION TO VALUE
    ----------------------------

    Uses the environment, so the type parameter for the environment in
    the monad is instantiated with Val.Env.t.
  *)
  let rec eval (expr : Ast.t) : (Val.any, Val.Env.t) m =
    let* () = incr_step ~max_step in
    match expr with
    (* concrete values *)
    | EUnit -> return_any VUnit
    | EInt i -> return_any (VInt (i, Formula.const_int i))
    | EBool b -> return_any (VBool (b, Formula.const_bool b))
    | EVar id -> fetch id
    | EFunction { param ; body } ->
      let* env = read in
      return_any (VFunClosure { param ; closure = { captured = body ; env }})
    | ERecord e_record_body ->
      let* record_body =
        Record.Label.Map.mapM (module Semantics) eval e_record_body
      in
      return_any (VRecord record_body)
    | EModule stmt_ls ->
      eval_statement_list stmt_ls
    | ETypeModule items ->
      let* env = read in
      return_any (VTypeModule { captured = items ; env })
    | ELet { stmt ; body } ->
      let* (binding, v) = eval_statement stmt in
      local (Env.set binding v) (eval body)
    | EAppl { func ; arg } ->
      let* v_func = force_eval func in
      begin match v_func with
      | Any (VFunClosure _ as vfun)
      | Any (VFunFix _ as vfun) ->
        let* v_arg = eval arg in
        eval_appl vfun v_arg
      | Any (VGenFun { funtype = { domain ; _ } ; _ } as vfun) ->
        let* v_arg = eval arg in
        fork_on_left ~reason:ApplGenFun
          ~left:(check v_arg domain)
          ~right:(eval_appl vfun v_arg)
      | Any (VWrapped { data ; tau } as self_fun) ->
        let* v_arg = eval arg in
        fork_on_left ~reason:ApplWrappedFun
          ~left:(check v_arg tau.domain)
          ~right:(
            let* v_res = eval_appl ~self_fun data v_arg in
            let* tval = eval_codomain tau.codomain v_arg in
            wrap v_res tval
          )
      | _ -> mismatch @@ apply_non_function v_func
      end
    | EMatch { subject ; patterns } ->
      let* v = force_eval subject in
      let rec find_match = function
        | [] -> mismatch @@ missing_pattern v (List.map fst patterns)
        | (pat, body) :: tl ->
          let* res = Matches.match_any pat v ~resolve_lazy in
          begin match res with
          | Match env' -> local (fun env -> Env.extend env env') (eval body)
          | No_match -> find_match tl
          | Failure msg -> escape (Mismatch msg)
          end
      in
      find_match patterns
    | EProject { record ; label } ->
      let* v = force_eval record in
      begin match v with
      | Any VRecord map_body
      | Any VModule map_body ->
        begin match Record.Label.Map.find_opt label map_body with
        | Some v' -> return v'
        | None -> mismatch @@ missing_label v label
        end
      | _ -> mismatch @@ project_non_record v label
      end
    | EVariant { label ; payload } ->
      let* v = eval payload in
      return_any (VVariant { label ; payload = v })
    | ETuple (e1, e2) ->
      let* v1 = eval e1 in
      let* v2 = eval e2 in
      return_any (VTuple (v1, v2))
    | EEmptyList ->
      return_any VEmptyList
    | EListCons { hd ; tl } ->
      let* hd = eval hd in
      let* v_tl = eval tl in (* don't force eval because want to allow cons to lazy list *)
      let cons_with_hd tl = return_any (VListCons { hd ; tl }) in
      begin match v_tl with
      | Any (VEmptyList as tl)
      | Any (VListCons _ as tl) -> cons_with_hd tl
      | Any (VLazy { cell ; _ } as tl) ->
        let* v_lazy = get_cell cell in
        begin match v_lazy with
        | LLazy LGenList _
        | LValue Any VEmptyList
        | LValue Any VListCons _ -> cons_with_hd tl
        | _ -> mismatch @@ cons_non_list hd v_tl
        end
      | _ -> mismatch @@ cons_non_list hd v_tl
      end
    | EAbstractType ->
      gen VType
    | ETypeSingle e ->
      let* v = eval e in
      return_any (VTypeSingle v)
    (* symbolic values and branching *)
    | ENot e ->
      let* v = force_eval e in
      begin match v with
      | Any VBool (b, s) -> return_any (VBool (not b, Smt.Formula.not_ s))
      | _ -> mismatch @@ not_non_bool v
      end
    | EBinop { left ; binop ; right } ->
      eval_binop left binop right
    | EIf { if_ ; then_ ; else_ } ->
      let* v = force_eval if_ in
      begin match v with
      | Any VBool (b, s) ->
        let cont = if b then then_ else else_ in
        let* () = push_formula_to_path (if b then s else Smt.Formula.not_ s) in
        eval cont
      | _ -> mismatch @@ if_non_bool v
      end
    | EAssert e ->
      let* v = force_eval e in
      begin match v with
      | Any VBool (b, s) ->
        if b then
          let* () = push_formula_to_path s in
          return_any VUnit
        else
          let* () = push_formula_to_path (Smt.Formula.not_ s) in
          escape Assert_false
      | _ -> mismatch @@ assert_non_bool v
      end
    | EAssume e ->
      let* v = force_eval e in
      begin match v with
      | Any VBool (b, s) ->
        if b then
          let* () = push_formula_to_path ~allow_flip:false s in
          return_any VUnit
        else
          let* () = push_formula_to_path (Smt.Formula.not_ s) in
          escape Vanish
      | _ -> mismatch @@ assume_non_bool v
      end
    (* types *)
    | EType -> return_any VType
    | ETypeInt -> return_any VTypeInt
    | ETypeBool -> return_any VTypeBool
    | ETypeTop -> return_any VTypeTop
    | ETypeBottom -> return_any VTypeBottom
    | ETypeUnit -> return_any VTypeUnit
    | ETypeRecord t_record_body ->
      let* record_body =
        Record.Label.Map.mapM (module Semantics) eval_type t_record_body
      in
      return_any (VTypeRecord record_body)
    | ETypeFun { domain = None, tau ; codomain } ->
      let* dom_t = eval_type tau in
      let* cod_t = eval_type codomain in
      return_any (VTypeFun { domain = dom_t ; codomain = CodValue cod_t })
    | ETypeFun { domain = Some id, tau ; codomain } ->
      let* dom_t = eval_type tau in
      let* env = read in
      return_any (VTypeFun { domain = dom_t
        ; codomain = CodDependent (id, { captured = codomain ; env }) })
    | ETypeRefine { var ; tau ; predicate } ->
      let* tval = eval_type tau in
      let* env = read in
      return_any (VTypeRefine { var ; tau = tval ; predicate = { captured = predicate ; env }})
    | ETypeMu { var ; body } ->
      let* env = read in
      return_any (VTypeMu { var ; closure = { captured = body ; env } })
    | ETypeList e ->
      let* t = eval_type e in
      return_any (VTypeList t)
    | ETypeVariant ls ->
      let* variant_bodies =
        List.fold_left (fun acc_m { Variant.label ; payload } ->
          let* acc = acc_m in
          let* tval = eval_type payload in
          return (Variant.Label.Map.add label tval acc)
        ) (return Variant.Label.Map.empty) ls
      in
      return_any (VTypeVariant variant_bodies)

  (*
    ----------------------------------
    EVALUATE BINARY OPERATION TO VALUE
    ----------------------------------

    Uses environment during evaluation.
  *)
  and eval_binop (left : Ast.t) (op : Binop.t) (right : Ast.t) : (Val.any, Val.Env.t) m =
    let* vleft = force_eval left in
    let eval_short_circuit vleft =
      match vleft with
      | Any VBool (b, s) when (not b && op = BAnd) || (b && op = BOr) ->
        (* Cases here are: false AND rhs, true OR rhs *)
        (* The short-circuiting is effectively a branch, so log the formula *)
        let* () = push_formula_to_path (Smt.Formula.binop Equal s (Smt.Formula.const_bool b)) in
        return vleft
      | Any VBool (b, s) ->
        (* Need to evaluate RHS here *)
        let* () = push_formula_to_path (Smt.Formula.binop Equal s (Smt.Formula.const_bool b)) in
        let* vright = force_eval right in
        begin match vright with
        | Any VBool _ -> return vright
        | _ -> mismatch @@ bad_binop vleft op vright
        end
      | _ -> mismatch @@ bad_binop vleft op (Any VUnit) (* placeholder because there is no expr printing yet *)
    in
    match op with
    | BAnd | BOr -> eval_short_circuit vleft
    | _ ->
      let* vright = force_eval right in
      let k f s1 s2 op =
        return_any @@ f (Smt.Formula.binop op s1 s2)
      in
      let v_int n s = VInt (n, s) in
      let v_bool n s = VBool (n, s) in
      match op, vleft, vright with
      | BPlus       , Any VInt (n1, e1) , Any VInt (n2, e2)  -> k (v_int (n1 + n2)) e1 e2 Plus
      | BMinus      , Any VInt (n1, e1) , Any VInt (n2, e2)  -> k (v_int (n1 - n2)) e1 e2 Minus
      | BTimes      , Any VInt (n1, e1) , Any VInt (n2, e2)  -> k (v_int (n1 * n2)) e1 e2 Times
      | BEqual      , Any VInt (n1, e1) , Any VInt (n2, e2)  -> k (v_bool (n1 = n2)) e1 e2 Equal
      | BEqual      , Any VBool (b1, e1), Any VBool (b2, e2) -> k (v_bool (b1 = b2)) e1 e2 Equal
      | BNeq        , Any VInt (n1, e1) , Any VInt (n2, e2)  -> k (v_bool (n1 <> n2)) e1 e2 Not_equal
      | BLessThan   , Any VInt (n1, e1) , Any VInt (n2, e2)  -> k (v_bool (n1 < n2)) e1 e2 Less_than
      | BLeq        , Any VInt (n1, e1) , Any VInt (n2, e2)  -> k (v_bool (n1 <= n2)) e1 e2 Less_than_eq
      | BGreaterThan, Any VInt (n1, e1) , Any VInt (n2, e2)  -> k (v_bool (n1 > n2)) e1 e2 Greater_than
      | BGeq        , Any VInt (n1, e1) , Any VInt (n2, e2)  -> k (v_bool (n1 >= n2)) e1 e2 Greater_than_eq
      | BDivide, Any VInt (n1, e1), Any VInt (n2, e2) when n2 <> 0 ->
        let* () = push_formula_to_path (Smt.Formula.binop Not_equal e2 (Smt.Formula.const_int 0)) in
        k (v_int (n1 / n2)) e1 e2 Divide
      | BModulus, Any VInt (n1, e1), Any VInt (n2, e2) when n2 <> 0 ->
        let* () = push_formula_to_path (Smt.Formula.binop Not_equal e2 (Smt.Formula.const_int 0)) in
        k (v_int (n1 mod n2)) e1 e2 Modulus
      | BTimes, v1, v2 ->
        (* Make tuple if v1 and v2 are types. Note that integer muliplication is handled above. *)
        handle_two v1 v2 (function
          | `Types (t1, t2) -> return_any @@ VTypeTuple (t1, t2)
          | _ -> mismatch @@ bad_binop vleft op vright
        )
      | _ -> mismatch @@ bad_binop vleft op vright

  (*
    ---------------------
    EVALUATE APPLICATIONS
    ---------------------

    Always takes the evaluation side. Does not do any checking.
    Does not push any labels corresponding to the evaluation.
    Does not wrap the result. Does not accept wrapped values as
    function to apply.

    ?self_fun is the optional value to put in the environment as
    the self for recursive functions, in case of wrapping.
    The default value is the actual fixed function.

    This does not use a monadic environment, so the environment is
    universally quantified.
  *)
  and eval_appl
    : 'env. Val.dval -> ?self_fun:Val.dval -> Val.any -> (Val.any, 'env) m
    = fun v_func ?(self_fun = v_func) v_arg ->
    match v_func with
    | VFunClosure { param ; closure = { captured ; env } } ->
      local' (Env.set param v_arg env) (eval captured)
    | VFunFix { fvar ; param ; closure = { captured ; env } } ->
      if do_splay && is_any_symbolic v_arg then
        mismatch @@ splayed_rec_fun v_func v_arg
      else
        local' (
          Env.set fvar (Any self_fun) env
          |> Env.set param v_arg
        ) (eval captured)
    | VGenFun { funtype = { domain ; codomain } ; table } ->
      let* mappings = get_cell table in
      let rec find_output = function
        | [] ->
          let* cod_tval = eval_codomain codomain v_arg in
          let* genned = gen cod_tval in
          let* cmp = make_comparable domain v_arg in
          let* () = set_cell table (mappings @ [(cmp, genned)]) in
          return genned
        | (cmp, output) :: tl ->
          let* (b, s) = extensional_equal cmp v_arg in
          if b then
            let* () = push_formula_to_path s in
            return output
          else
            let* () = push_formula_to_path (Formula.not_ s) in
            find_output tl
      in
      find_output mappings
    | _ -> mismatch @@ apply_non_function (Any v_func)

  (*
    ---------------------------------
    EVALUATE EXPRESSION TO TYPE VALUE
    ---------------------------------

    Uses environment to evaluate.
  *)
  and eval_type (expr : Ast.t) : (Val.tval, Val.Env.t) m =
    let* v = force_eval expr in
    handle_any v
      ~dat:(fun d -> mismatch @@ non_type_value d)
      ~typ:return

  (*
    -----------------------------------------------
    EVALUATE RECURSIVE TYPE TO A NON-REC TYPE VALUE
    -----------------------------------------------

    Fails if a cycle is detected. Example cycles include
      mu t. t (* 1-cycle *)
    and
      mu t. let s = mu s. t in s (* 2-cycle *)
    A cycle is not detected in
      mu t. { a : int ; b : t }
    and non-splayed any generation of it will diverge.
  *)
  and unroll_mu
    : 'env. Ident.t -> Ast.t Val.closure -> (Val.tval, 'env) m
    = fun var closure ->
    let rec go seen var closure =
      let t = VTypeMu { var ; closure } in
      let* t_body = local' (Env.set var (Any t) closure.env) (eval_type closure.captured) in
      match t_body with
      | VTypeMu { var ; closure } ->
        (* Check for cycle by looking for this type in what we've seen before *)
        if List.exists (Val.equal_closure closure) seen then
          mismatch @@ non_contractive_type t_body
        else
          go (closure :: seen) var closure
      | _ ->
        return t_body
    in
    go [] var closure

  (*
    --------------------------
    EVALUATE FUNCTION CODOMAIN
    --------------------------

    Given a witness value of the domain type, evaluate the codomain
    (whether it is already a type value or it depends on the witness)
    to a type value.

    Does not use environment.
  *)
  and eval_codomain
    : 'env. Val.fun_cod -> Val.any -> (Val.tval, 'env) m
    = fun cod dom_witness ->
    match cod with
    | CodValue cod_tval ->
      return cod_tval
    | CodDependent (id, { captured ; env }) ->
      local' (Env.set id dom_witness env) (eval_type captured)

  (*
    -------------------------
    CHECK FOR TYPE REFUTATION
    -------------------------

    Does not use environment.
  *)
  and check
    : 'a 'env. Val.any -> Val.tval -> ('a, 'env) m
    = fun v t ->
    let refute = escape (Refutation (v, t)) in
    let confirm = escape Confirmation in
    let* () = incr_step ~max_step in
    (* In just about every case except checking mu type, we want to force the value. *)
    (* Even though it is wordy, we do this forcing inside each case. *)
    match t with
    | VTypeInt ->
      let* v = force_value v in
      begin match v with
      | Any VInt _ -> confirm
      | _ -> refute
      end
    | VTypeBool ->
      let* v = force_value v in
      begin match v with
      | Any VBool _ -> confirm
      | _ -> refute
      end
    | VTypeUnit ->
      let* v = force_value v in
      begin match v with
      | Any VUnit -> confirm
      | _ -> refute
      end
    | VTypeTop -> (* don't force v *)
      (* Everything is in top *)
      confirm
    | VTypeBottom -> (* don't force v *)
      (* Nothing is in bottom *)
      refute
    | VTypePoly { id } ->
      let* v = force_value v in
      begin match v with
      | Any VGenPoly { id = id' ; nonce = _ } when id = id' -> confirm
      | _ -> refute
      end
    | VType ->
      let* v = force_value v in
      handle_any v ~dat:(fun _ -> refute) ~typ:(fun _ -> confirm)
    | VTypeFun { domain ; codomain } ->
      let* v = force_value v in
      begin match v with
      | Any (VFunClosure _ as vfun)
      | Any (VFunFix _ as vfun) ->
        let* genned = gen domain in
        let* res = eval_appl vfun genned in
        let* cod_tval = eval_codomain codomain genned in
        check res cod_tval
      | Any (VGenFun { funtype = { domain = domain' ; codomain = codomain' } ; _ } as v_candidate) ->
        fork_on_left ~reason:CheckGenFun
          ~left:(domain <: domain')
          ~right:(
            if Val.equal_fun_cod codomain codomain' then confirm else
            let* v_arg = gen domain in
            (*
              Since we can assume domain <: domain', it's possible
              that codomain' can misuse genned with respect to
              domain. We must therefore wrap genned with domain to
              check that codomain' does not misuse it.
            *)
            let* w_arg = wrap v_arg domain' in
            let* res = eval_appl v_candidate w_arg in
            let* cod_tval = eval_codomain codomain w_arg in
            check res cod_tval
          )
      | Any (VWrapped { data ; tau = { domain = domain' ; codomain = codomain' } } as self_fun) ->
        fork_on_left ~reason:CheckWrappedFun
          ~left:(domain <: domain')
          ~right:(
            (*
              The left has already checked the domain, so we can assume the
              domain side is well-typed.

              We can skip the work on the right if the codomains are equal
                because the wrapper means it has been checked.
            *)
            if Val.equal_fun_cod codomain codomain' then confirm else
            match data with
            | VFunClosure _
            | VFunFix _ ->
              let* v_arg = gen domain in
              let* cod_tval = eval_codomain codomain v_arg in
              let* w_arg = wrap v_arg domain' in
              let* res = eval_appl data ~self_fun v_arg in
              let* cod_tval' = eval_codomain codomain' w_arg in
              let* w_res = wrap res cod_tval' in
              check w_res cod_tval
            | VGenFun { funtype = { domain = domain'' ; codomain = codomain'' } ; _ } ->
              fork_on_left ~reason:CheckGenFun
                ~left:(domain <: domain'')
                ~right:(
                  let* v_arg = gen domain in
                  let* w_arg = wrap v_arg domain' in
                  let* res = eval_appl data w_arg in
                  (*
                    Since codomain'' has already been evaluated depending on any
                    v in domain' wrapped with domain'', we know that codomain''
                    does not misuse any value in domain'' with respect to the type
                    domain'. Hence there is no need to wrap with domain' before
                    evaluating codomain'' because it cannot possibly go wrong.
                  *)
                  let* cod_tval'' = eval_codomain codomain'' w_arg in
                  let* w = wrap res cod_tval'' in
                  let* cod_tval = eval_codomain codomain v_arg in
                  check w cod_tval
                )
            | _ -> refute
          )
      | _ -> refute
      end
    | VTypeVariant variant_t ->
      let* v = force_value v in
      begin match v with
      | Any VVariant { label ; payload } ->
        begin match Variant.Label.Map.find_opt label variant_t with
        | Some t -> check payload t
        | None -> refute
        end
      | _ -> refute
      end
    | VTypeRecord record_t ->
      let* v = force_value v in
      begin match v with
      | Any VRecord record_v ->
        let t_labels = Record.label_set record_t in
        let v_labels = Record.label_set record_v in
          let push_and_check label =
            let* () = push_and_log_tag (Grammar.Tag.of_record_label Check label) in
            check
              (Record.Label.Map.find label record_v)
              (Record.Label.Map.find label record_t)
          in
          check_struct push_and_check ~refute ~t_labels ~v_labels
      | _ -> refute
      end
    | VTypeModule { captured ; env } ->
      let* v = force_value v in
      begin match v with
      | Any VModule module_v ->
        let t_labels_ls = List.map fst captured in
        let t_labels = Record.Label.Set.of_list t_labels_ls in
        let v_labels = Record.label_set module_v in
        let push_and_check label =
          let* () = push_and_log_tag (Grammar.Tag.of_record_label Check label) in
          let new_env, tau =
            (* think about sharing this computation because rn it is redone on every fork *)
            Utils.List_utils.fold_until (fun env (label', tau) ->
              if Record.Label.equal label' label
              then `Stop (env, tau)
              else `Continue (
                Env.set (Record.Label.to_ident label') (Record.Label.Map.find label' module_v) env
              )
            ) (fun _ -> raise @@ InvariantException "Label not found in module type") env captured
          in
          let* t = local' new_env (eval_type tau) in
          check (Record.Label.Map.find label module_v) t
        in
        check_struct push_and_check ~refute ~t_labels ~v_labels
      | _ -> refute
      end
    | VTypeMu { var ; closure = ({ captured ; env } as closure) } -> (* don't force v *)
      (* Begin by unrolling to ensure the type is contractive.
        Noncontractive types are disallowed cause an error. *)
      let* t_body = unroll_mu var closure in
      begin match v with
      | Any VLazy { cell ; wrapping_types } ->
        let* lazy_v = get_cell cell in
        begin match lazy_v with
        | LValue any_v ->
          check any_v t
        | LLazy LGenList _ ->
          check v t_body
        | LLazy LGenMu { var = var' ; closure = { captured = captured' ; env = env' } } ->
          let* a = gen VType in (* fresh type to use as a stub *)
          let* t_body = local' (Env.set var a env) (eval_type captured) in
          let* t_body' = local' (Env.set var' a env') (eval_type captured') in
          if Val.equal t_body t_body' && wrapping_types = [] then confirm else
          let* genned = gen t_body' in
          let* wrapped = wrap_multi wrapping_types genned in
          check wrapped t_body
        end
      | _ -> check v t_body
      end
    | VTypeList t_body -> (* don't force v *)
      begin match v with
      | Any VLazy { cell ; wrapping_types } ->
        let* lazy_v = get_cell cell in
        begin match lazy_v with
        | LValue any_v ->
          let* wrapped = wrap_multi wrapping_types any_v in
          check wrapped t
        | LLazy LGenMu { var ; closure } ->
          (* Unroll the type and check to see if it is a list. *)
          let* tval_mu_body = unroll_mu var closure in
          tval_mu_body <: t
        | LLazy LGenList t' ->
          if wrapping_types = [] && Val.equal t' t_body then confirm else
          let* genned = gen t' in
          (* genned is only a single element of the list, so wrap it
            by extracting the type bodies out of the list type *)
          let* wrapping_bodies =
            List.fold_right (fun twrap acc_m ->
              let* acc = acc_m in
              match twrap with
              | VTypeList tval -> return (tval :: acc)
              | _ -> mismatch "Wrap list with non-list type"
            ) wrapping_types (return [])
          in
          let* wrapped = wrap_multi wrapping_bodies genned in
          check wrapped t_body
        end
      | Any VEmptyList -> confirm
      | Any VListCons { hd ; tl } ->
        fork_on_left ~reason:CheckList
          ~left:(check hd t_body)
          ~right:(check (Any tl) t)
      | _ -> refute
      end
    | VTypeRefine { var ; tau ; predicate = { captured ; env } } ->
      (* Value is not directly used here, so we don't force it quite yet *)
      fork_on_left ~reason:CheckRefinementType
        ~left:(check v tau)
        ~right:(
          let* p = local' (Env.set var v env) (eval captured) in
          match p with
          | Any VBool (b, s) ->
            if b then
              let* () = push_formula_to_path s in
              confirm
            else
              let* () = push_formula_to_path ~allow_flip:false (Smt.Formula.not_ s) in
              refute
          | _ -> mismatch @@ non_bool_predicate p
        )
    | VTypeTuple (t1, t2) ->
      let* v = force_value v in
      begin match v with
      | Any VTuple (v1, v2) ->
        fork_on_left ~reason:CheckTuple
          ~left:(check v1 t1)
          ~right:(check v2 t2)
      | _ -> refute
      end
    | VTypeSingle v_single ->
      let* v = force_value v in
      handle_two v_single v (function
        | `Types (tval, tval') ->
          (* For type equality, check subsets *)
          if Val.equal tval' tval then confirm else
          fork_on_left ~reason:CheckSingleton
            ~left:(tval' <: tval)
            ~right:(tval <: tval')
        | _ ->
          (* For non-type equality, use intensional equality *)
          let (b, s) = Val.intensional_equal v_single v in
          if b then
            let* () = push_formula_to_path s in
            confirm
          else
            let* () = push_formula_to_path ~allow_flip:false (Formula.not_ s) in
            refute
      )

  (*
    Check modules and records given a way to check each label and a default label.
  *)
  and check_struct
    : type a env. ('any. Record.Label.t -> ('any, env) m) -> refute:(a, env) m ->
      t_labels:Record.Label.Set.t -> v_labels:Record.Label.Set.t -> (a, env) m
    = fun check_label ~refute ~t_labels ~v_labels ->
      if Record.Label.Set.subset t_labels v_labels then
        (* incr step because about to read an input *)
        let* () = incr_step ~max_step in
        let* l_opt = read_input KTag input_env in
        match l_opt with
        | Some Label (id, Check) -> (check_label (Record.Label.RecordLabel id))
        | Some _ -> raise bad_input_env
        | None ->
          (* is in exploration mode, so we want to check every label *)
          let rec go enum =
            match Record.Label.Set.Enum.head_opt enum with
            | Some label ->
              let* () = fork (check_label label) in
              go (Record.Label.Set.Enum.tail enum)
            | None -> escape Eval_result.Confirmation
          in
          go (Record.Label.Set.Enum.enum t_labels)
      else
        refute

  (*
    -------------
    CHECK SUBTYPE
    -------------

    [t1 <: t2] can be a refutation if t1 is not
      a subtype of t2. It is a confirmation on failure to
      find such refutation.

    Does not use the environment.
  *)
  and (<:)
    : 'a 'env. Val.tval -> Val.tval -> ('a, 'env) m
    = fun t1 t2 ->
      if Val.equal t1 t2 then
        escape Confirmation
      else
        let* genned = gen t1 in
        check genned t2

  (*
    -------------------------
    GENERATE MEMBER OF A TYPE
    -------------------------

    Does not use the environment.
  *)
  and gen
    : 'env. Val.tval -> (Val.any, 'env) m
    = fun t ->
    let* () = incr_step ~max_step in
    match t with
    | VTypeUnit ->
      return_any VUnit
    | VTypeInt ->
      let* step = step in
      let* i = read_and_log_input KInt input_env ~default:(default_int ()) in
      return_any (VInt (i, Stepkey.int_symbol step))
    | VTypeBool ->
      let* step = step in
      let* b = read_and_log_input KBool input_env ~default:(default_bool ()) in
      return_any (VBool (b, Stepkey.bool_symbol step))
    | VTypeFun funtype ->
      let* table = new_cell [] in
      return_any (VGenFun { funtype ; table })
    | VType ->
      let* Step id = step in (* will use step for a fresh integer *)
      return_any (VTypePoly { id })
    | VTypePoly { id } ->
      let* Step nonce = step in (* will use step for a fresh nonce *)
      return_any (VGenPoly { id ; nonce })
    | VTypeTop ->
      (* parametric polymorphism is enough here *)
      let* newtype = gen VType in
      handle_any newtype
        ~dat:(fun _ -> raise @@ InvariantException "`type` generated data value")
        ~typ:gen
    | VTypeBottom -> escape Vanish
    | VTypeRecord record_t ->
      let* genned_body =
        Record.Label.Map.mapM (module Semantics) gen record_t
      in
      return_any (VRecord genned_body)
    | VTypeVariant variant_t ->
      let t_labels = Variant.Label.B.domain variant_t in
      let* l =
        read_and_log_input KTag input_env
          ~default:(default_constructor variant_t |> Grammar.Tag.of_variant_label Gen)
      in
      begin match l with
      | Label (id, Gen) ->
        let to_gen = Variant.Label.of_ident id in
        let t = Variant.Label.Map.find to_gen variant_t in
        let* () =
          push_tag_to_path l
            ~alternatives:(
              Variant.Label.Set.remove to_gen t_labels
              |> Variant.Label.Set.list_map (Grammar.Tag.of_variant_label Gen)
            )
        in
        let* payload = gen t in
        return_any (VVariant { label = to_gen ; payload })
      | _ -> raise bad_input_env
      end
    | VTypeList t ->
      if do_splay then
        let* l = new_lazy_cell (LGenList t) in
        return_any l
      else
        force_gen_list t
    | VTypeRefine { var ; tau ; predicate = { captured ; env } } ->
      let* v = gen tau in
      let* p = local' (Env.set var v env) (eval captured) in
      begin match p with
      | Any VBool (true, s) ->
        let* () = push_formula_to_path ~allow_flip:false s in
        return v
      | Any VBool (false, s) ->
        let* () = push_formula_to_path (Smt.Formula.not_ s) in
        escape Vanish
      | _ -> mismatch @@ non_bool_predicate p
      end
    | VTypeMu { var ; closure } ->
      if do_splay then
        let* lgen = new_lazy_cell (LGenMu { var ; closure }) in
        return_any lgen
      else
        force_gen_mu var closure
    | VTypeTuple (t1, t2) ->
      let* v1 = gen t1 in
      let* v2 = gen t2 in
      return_any (VTuple (v1, v2))
    | VTypeModule { captured ; env } ->
      let rec fold_labels acc_m = function
        | [] -> acc_m
        | (label, tau) :: tl ->
          let* acc = acc_m in
          let* tval = eval_type tau in
          let* v = gen tval in
          local (Env.set (Record.Label.to_ident label) v) (
            fold_labels (return @@ Record.Label.Map.add label v acc) tl
          )
      in
      let* genned_body =
        local' env (
          fold_labels (return Record.Label.Map.empty) captured
        )
      in
      return_any (VModule genned_body)
    | VTypeSingle v ->
      return v

  (*
    Generate a list. Makes an actual list instead of a symbol for a lazy one.

    Does not use the environment.
  *)
  and force_gen_list
    : 'env. Val.tval -> (Val.any, 'env) m
    = fun body ->
    let* l = read_and_log_input KTag input_env ~default:(Left GenList) in
    match l with
    | Left GenList ->
      let* () = push_tag_to_path (Left GenList) ~alternatives:[ Right GenList ] in
      let* () = incr_step ~max_step in (* doesn't call gen, so need to increment step manually *)
      return_any VEmptyList
    | Right GenList ->
      let* () = push_tag_to_path (Right GenList) ~alternatives:[ Left GenList ] in
      let* hd = gen body in
      let* Any v_tl = gen (VTypeList body) in
      handle v_tl
        ~dat:(fun tl -> return_any @@ VListCons { hd ; tl })
        ~typ:(fun _ -> raise @@ InvariantException "List generation makes a type value")
    | _ -> raise bad_input_env

  (*
    Generate a member of a recursive type. Does not make a symbol for a lazy member.

    Does not use the environment.
  *)
  and force_gen_mu
    : 'env. Ident.t -> Ast.t Val.closure -> (Val.any, 'env) m
    = fun var closure ->
    let* t_body = unroll_mu var closure in
    match t_body with
    | VTypeList t -> force_gen_list t
    | _ -> gen t_body (* not mu type because of behavior of unroll_mu *)

  (*
    ----
    WRAP
    ----

    Does not use the environment.

    Does not fail with any type mismatches if the wrapping type
    is wrong for the value. In such a case, it simply returns the
    value unaffected. This is useful for checking recursive types
    by putting a polymorphic type in place of the recursive type,
    and letting wrapping gloss over that polymorphic value.
  *)
  and wrap
    : 'env. Val.any -> Val.tval -> (Val.any, 'env) m
    = fun v t ->
    if not do_wrap then return v else
    match t with
    | VType
    | VTypePoly _
    | VTypeUnit
    | VTypeTop
    | VTypeInt
    | VTypeBool
    | VTypeSingle _ -> return v
    | VTypeBottom -> mismatch @@ wrap_bottom v
    | VTypeMu { var ; closure } ->
      let* tval = unroll_mu var closure in
      begin match v with
      | Any VLazy vlazy ->
        (* Always lazily wrap, even if the value is forced already. *)
        (* It is safe to put this off because the act itself of wrapping
          is never the sole way to find an error. *)
        if does_wrap_matter tval then
          return_any (VLazy { vlazy with wrapping_types = tval :: vlazy.wrapping_types })
        else
          return v
      | _ ->
        wrap v tval
      end
    | VTypeList t_body ->
      begin match v with
      | Any VLazy vlazy when does_wrap_matter t ->
        return_any (VLazy { vlazy with wrapping_types = t :: vlazy.wrapping_types })
      | Any VListCons { hd ; tl } ->
        let* w_hd = wrap hd t_body in
        let* Any w_tl = wrap (Any tl) t in
        handle w_tl
          ~dat:(fun w_tl_data ->
            if w_hd == hd && w_tl_data == tl then
              return v
            else
              return_any (VListCons { hd = w_hd ; tl = w_tl_data })
          )
          ~typ:(fun _ -> raise @@ InvariantException "Wrapped list is not data")
      | Any VLazy _ (* wrap must not matter due to pattern guard above *)
      | Any VEmptyList (* wrapping empty list does nothing *)
      | _ -> (* ignore mismatches, and just do nothing *)
        return v
      end
    | VTypeFun tfun ->
      begin match v with
      | Any VWrapped { data ; tau = _ } ->
        return_any (VWrapped { data ; tau = tfun })
      | Any v' ->
        handle v'
          ~dat:(fun data -> return_any (VWrapped { data ; tau = tfun }))
          ~typ:(fun _ -> return v)
      end
    | VTypeRecord t_body ->
      begin match v with
      | Any VRecord v_body ->
        let wrap_one l t =
          match Record.Label.Map.find_opt l v_body with
          | Some v' -> wrap v' t
          | None -> mismatch (missing_label v l)
        in
        let* w_body =
          Record.Label.Map.mapiM (module Semantics) wrap_one t_body
        in
        return_any (VRecord w_body)
      | _ ->
        return v
      end
    | VTypeModule { captured = t_ls ; env } ->
      begin match v with
      | Any VModule v_body ->
        let rec fold_labels acc_m = function
          | [] -> acc_m
          | (label, tau) :: tl ->
            let* acc = acc_m in
            begin match Record.Label.Map.find_opt label v_body with
            | Some v' ->
              let* tval = eval_type tau in
              let* v = wrap v' tval in
              local (Env.set (Record.Label.to_ident label) v) (
                fold_labels (return @@ Record.Label.Map.add label v acc) tl
              )
            | None ->
              return acc
            end
        in
        let* wrapped_body =
          local' env (
            fold_labels (return Record.Label.Map.empty) t_ls
          )
        in
        return_any (VModule wrapped_body)
      | _ ->
        return v
      end
    | VTypeVariant t_body ->
      begin match v with
      | Any VVariant { label ; payload } ->
        begin match Variant.Label.Map.find_opt label t_body with
        | Some t ->
          let* w = wrap payload t in
          if w == payload then
            return v (* return value unchanged because wrapping did nothing *)
          else
            return_any (VVariant { label ; payload = w })
        | None ->
          return v
        end
      | _ ->
        return v
      end
    | VTypeTuple (t1, t2) ->
      begin match v with
      | Any VTuple (v1, v2) ->
        let* w1 = wrap v1 t1 in
        let* w2 = wrap v2 t2 in
        if w1 == v1 && w2 == v2 then
          return v (* return value unchanged because wrapping did nothing *)
        else
          return_any (VTuple (w1, w2))
      | _ ->
        return v
      end
    | VTypeRefine { var = _ ; tau ; predicate = _ } ->
      wrap v tau

  (*
    Wrap with FIFO queue of types, represented as a list.
    That is, the last type in the list wraps first.
  *)
  and wrap_multi
    : 'env. Val.tval list -> Val.any -> (Val.any, 'env) m
    = fun queue v ->
    List.fold_right (fun twrap acc_m ->
      let* acc = acc_m in
      wrap acc twrap
    ) queue (return v)

  (*
    ---------------------------------------
    EVALUATE LIST OF STATEMENTS TO A MODULE
    ---------------------------------------

    Uses the environment when evaluating.
  *)
  and eval_statement_list (statements : Ast.statement list) : (Val.any, Val.Env.t) m =
    let rec fold_stmts acc_m = function
      | [] -> acc_m
      | stmt :: tl ->
        let* acc = acc_m in
        let* (id, v) = eval_statement stmt in
        local (Env.set id v) (
          fold_stmts (return @@ Record.Label.Map.add (Record.Label.of_ident id) v acc) tl
        )
    in
    let* module_body =
      fold_stmts (return Record.Label.Map.empty) statements
    in
    return_any (VModule module_body)

  (*
    -------------------------------
    EVALUATE STATEMENT TO A BINDING
    -------------------------------

    Uses the environment when evaluating.
  *)
  and eval_statement (stmt : Ast.statement) : (Ident.t * Val.any, Val.Env.t) m =
    match stmt with
    | SLet { name ; annot = ANone ; defn } ->
      let* v = eval defn in
      return (name, v)
    | SLetRec { name ; annot = ANone ; param ; defn } ->
      let* env = read in
      let v = to_any (VFunFix { fvar = name ; param ; closure = { captured = defn ; env } }) in
      return (name, v)
    | SLet { name ; annot = AType { tau ; do_check } ; defn } ->
      let* tval = eval_type tau in
      let* v = eval defn in
      let wrapped_val =
        let* w = wrap v tval in
        return (name, w)
      in
      if do_check then
        fork_on_left ~reason:CheckLetExpr
          ~left:(check v tval)
          ~right:wrapped_val
      else
        wrapped_val
    | SLetRec { name ; annot = AType { tau ; do_check } ; param ; defn } ->
      let* tval = eval_type tau in
      let* env = read in
      let* v =
        let* self =
          if do_splay then
            gen tval
          else
            wrap (Any (
              VFunFix { fvar = name ; param ; closure = { captured = defn ; env } }
            )) tval
        in
        (* we don't just return a wrapped fix fun because that would skip the check *)
        return_any (VFunClosure { param ; closure =
          { captured = defn ; env = Env.set name self env } }
        )
      in
      let wrapped_val =
        let* w = wrap v tval in
        return (name, w)
      in
      if do_check then
        fork_on_left ~reason:CheckLetExpr
          ~left:(check v tval)
          ~right:wrapped_val
      else
        wrapped_val

  (*
    -------------------------------
    EVALUATE SYMBOLS TO WHNF VALUES
    -------------------------------

    Uses the environment when evaluating.
  *)
  and force_eval (expr : Ast.t) : (Val.any, Val.Env.t) m =
    let* v = eval expr in
    force_value v

  (*
    --------------------
    FORCE VALUES TO WHNF
    --------------------

    Does not use the environment.
  *)
  and force_value
    : 'env. Val.any -> (Val.any, 'env) m
    = fun v ->
    if do_splay then
      match v with
      | Any VLazy vlazy -> resolve_lazy vlazy
      | _ -> return v
    else
      (* without splaying, nothing is ever delayed because it would be incomplete *)
      return v

  (*
    Forces the value to weak head normal form and wraps
    with any lazily-done wrappings.
  *)
  and resolve_lazy
    : 'env. Val.lazy_cell -> (Val.any, 'env) m
    = fun { cell ; wrapping_types } ->
    assert do_splay;
    let* v_any =
      let* lazy_v = get_cell cell in
      match lazy_v with
      | LLazy lv ->
        let* genned =
          match lv with
          | LGenMu { var ; closure } -> force_gen_mu var closure
          | LGenList t -> force_gen_list t
        in
        let* () = set_cell cell (LValue genned) in
        return genned
      | LValue v_any ->
        return v_any
    in
    wrap_multi wrapping_types v_any

  (*
    --------------------
    EXTENSIONAL EQUALITY
    --------------------
  *)
  and extensional_equal
    : type env. Val.comparable -> Val.any -> (bool Cdata.t, env) m
    = fun c v ->
    let open Semantics.Comparator (struct type nonrec env = env end) in
    match v with
    | Any VLazy { cell ; wrapping_types = _ } ->
      let* vlazy = get_cell cell in
      begin match vlazy with
      | LValue any ->
        (* The lazy value has been pulled on, so use known value *)
        extensional_equal c any
      | LLazy _ ->
        (* The value is lazy. Do not pull on it. *)
        begin match c with
        | CLazy cmp_cell ->
          (* The comparable is also lazy. See if it has been pulled on. *)
          let* cmp_lazy = get_cell cmp_cell in
          begin match cmp_lazy with
          | LWaiting (cell', _) ->
            (* Incomplete: these values _could_ be equal, but we say they are
              equal here only if they are the same cell. In most cases, they
              are not equal. *)
            make (cell = cell')
          | LComp cmp ->
            (* The comparable has been pulled on. Continue with this one. *)
            extensional_equal cmp v
          end
        | _ ->
          (* Do not pull on values. Say false. *)
          make false
        end
      end
    | _ ->
      match c with
      | CSingle -> return Cdata.true_
      | CIntensional v' ->
        return (Val.intensional_equal v v')
      | CFun { tfun = { domain ; codomain } ; mapping } ->
        let* comp_fun = get_cell mapping in
        let eq arg mapsto =
          Val.handle_any v ~dat:(fun f ->
            let* res = eval_appl (Val.discard_wrapper f) arg in
            extensional_equal mapsto res
          ) ~typ:(fun _ -> make false)
        in
        begin match comp_fun with
        | FWaiting v_func ->
          let* () = incr_step ~max_step in
          let* tag = read_and_log_input KTag input_env ~default:(Left ChooseEmptyFun) in
          begin match tag with
          | Left ChooseEmptyFun ->
            let* () = push_tag_to_path ~alternatives:[ Right ChooseEmptyFun ] tag in
            let* () = set_cell mapping FEmpty in
            make true
          | Right ChooseEmptyFun ->
            let* () = push_tag_to_path ~alternatives:[ Left ChooseEmptyFun ] tag in
            let* arg = gen domain in
            let* result = eval_appl v_func arg in
            let* cod_tval = eval_codomain codomain arg in
            let* mapsto = make_comparable cod_tval result in
            let* () = set_cell mapping (FMapping { arg ; mapsto }) in
            eq arg mapsto
          | _ ->
            raise bad_input_env
          end
        | FMapping { arg ; mapsto } ->
          eq arg mapsto
        | FEmpty ->
          (* Everything equals the empty table *)
          make true
        end
      | CLazy cell ->
        let* cmp_lazy = get_cell cell in
        begin match cmp_lazy with
        | LComp cmp -> extensional_equal cmp v
        | LWaiting (cmp_v_cell, t) ->
          let* vlazy = get_cell cmp_v_cell in
          begin match vlazy with
          | LValue v_cmp ->
            (* The value has been pulled on, so we have enough information now
              to construct a comparable. Do so, update the cell, and use it. *)
            let* cmp = make_comparable t v_cmp in
            let* () = set_cell cell (LComp cmp) in
            extensional_equal cmp v
          | LLazy _ ->
            make false (* the v = lazy case is handled above *)
          end
        end
      | CTuple (c1, c2) ->
        begin match v with
        | Any VTuple (v1, v2) ->
          let- () = extensional_equal c1 v1 in
          extensional_equal c2 v2
        | _ -> make false
        end
      | CEmptyList ->
        begin match v with
        | Any VEmptyList -> make true
        | _ -> make false
        end
      | CListCons (c_hd, c_tl) ->
        begin match v with
        | Any VListCons { hd ; tl } ->
          let- () = extensional_equal c_hd hd in
          extensional_equal c_tl (Any tl)
        | _ -> make false
        end
      | CRecord m ->
        begin match v with
        | Any VRecord record_body ->
          Record.fold (fun l cmp acc ->
            let- () = acc in
            match Record.Label.Map.find_opt l record_body with
            | Some x -> extensional_equal cmp x
            | _ -> mismatch "missing record label"
          ) (make true) m
        | _ -> make false
        end
      | CVariant { label = c_label ; payload = c_payload } ->
        begin match v with
        | Any VVariant { label ; payload } ->
          let= () = Variant.Label.equal c_label label in
          extensional_equal c_payload payload
        | _ ->
          make false
        end

  and make_comparable
    : 'env. Val.tval -> Val.any -> (Val.comparable, 'env) m
    = fun t v ->
    match v with
    | Any VLazy { cell ; wrapping_types = _ } ->
      let* vlazy = get_cell cell in
      begin match vlazy with
      | LLazy _ ->
        let* cmp_cell = new_cell (LWaiting (cell, t)) in
        return (CLazy cmp_cell)
      | LValue any -> make_comparable t any
      end
    | _ ->
      match t with
      | VType
      | VTypeUnit
      | VTypeTop
      | VTypeInt
      | VTypeBool
      | VTypePoly _
      | VTypeModule _ -> return (CIntensional v)
      | VTypeSingle _ -> return CSingle
      | VTypeBottom -> escape (Refutation (v, t))
      | VTypeMu { var ; closure } ->
        let* t_body = unroll_mu var closure in
        make_comparable t_body v
      | VTypeList t_body ->
        begin match v with
        | Any VEmptyList -> return CEmptyList
        | Any VListCons { hd ; tl } ->
          let* c_hd = make_comparable t_body hd in
          let* c_tl = make_comparable t (Any tl) in
          return (CListCons (c_hd, c_tl))
        | _ -> vanish
        end
      | VTypeFun tfun ->
        handle_any v ~dat:(fun f ->
          let* cell = new_fun_cell (Val.discard_wrapper f) in
          return (CFun { tfun ; mapping = cell })
        ) ~typ:(fun _ -> vanish)
      | VTypeRecord m ->
        begin match v with
        | Any VRecord record_body ->
          let mk l t =
            match Record.Label.Map.find_opt l record_body with
            | Some v_body -> make_comparable t v_body
            | None -> vanish
          in
          let* c_rec = Record.Label.Map.mapiM (module Semantics) mk m in
          return (CRecord c_rec)
        | _ -> vanish
        end
      | VTypeVariant m ->
        begin match v with
        | Any VVariant { label ; payload } ->
          begin match Variant.Label.Map.find_opt label m with
          | Some t_body ->
            let* cmp = make_comparable t_body payload in
            return (CVariant { label ; payload = cmp })
          | None -> vanish
          end
        | _ -> vanish
        end
      | VTypeRefine { var = _ ; tau ; predicate = _ } ->
        make_comparable tau v
      | VTypeTuple (t1, t2) ->
        begin match v with
        | Any VTuple (v1, v2) ->
          let* c1 = make_comparable t1 v1 in
          let* c2 = make_comparable t2 v2 in
          return (CTuple (c1, c2))
        | _ -> vanish
        end

  in

  let result, state = run (eval_statement_list pgm) target in
  let answer = Eval_result.to_answer result in
  { target ; rev_stem = state.rev_stem ; answer } :: state.runs
