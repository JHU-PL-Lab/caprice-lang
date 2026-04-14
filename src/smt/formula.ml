
module type S = sig
  type ('a, 'k) t

  val equal : ('a, 'k) t -> ('a, 'k) t -> bool

  val const_int : int -> (int, 'k) t
  val const_bool : bool -> (bool, 'k) t

  val symbol : ('a, 'k) Symbol.t -> ('a, 'k) t

  val not_ : (bool, 'k) t -> (bool, 'k) t

  val binop : ('a * 'a * 'b) Binop.t -> ('a, 'k) t -> ('a, 'k) t -> ('b, 'k) t

  val is_const : ('a, 'k) t -> bool

  val and_ : (bool, 'k) t list -> (bool, 'k) t
end

module T : sig
  type (_, 'k) t = private
    | Const_int : int -> (int, 'k) t
    | Const_bool : bool -> (bool, 'k) t
    | Key : ('a, 'k) Symbol.t -> ('a, 'k) t
    | Not : (bool, 'k) t -> (bool, 'k) t
    | And : (bool, 'k) t list -> (bool, 'k) t
    | Binop : ('a * 'a * 'b) Binop.t * ('a, 'k) t * ('a, 'k) t -> ('b, 'k) t

  include S with type ('a, 'k) t := ('a, 'k) t
end = struct
  type (_, 'k) t =
    | Const_int : int -> (int, 'k) t
    | Const_bool : bool -> (bool, 'k) t
    | Key : ('a, 'k) Symbol.t -> ('a, 'k) t
    | Not : (bool, 'k) t -> (bool, 'k) t
    | And : (bool, 'k) t list -> (bool, 'k) t
    | Binop : ('a * 'a * 'b) Binop.t * ('a, 'k) t * ('a, 'k) t -> ('b, 'k) t

  let rec equal : type a. (a, 'k) t -> (a, 'k) t -> bool = fun x y ->
    x == y || poly_equal x y

  and poly_equal : type a b. (a, 'k) t -> (b, 'k) t -> bool = fun x y ->
    match x, y with
    | Const_int i, Const_int j -> i = j
    | Const_bool b, Const_bool c -> Bool.equal b c
    | Key I k, Key I k' -> Utils.Uid.equal k k'
    | Key B k, Key B k' -> Utils.Uid.equal k k'
    | Not e, Not e' -> equal e e'
    | And l, And l' -> List.equal equal l l'
    | Binop (b, l, r), Binop (b', l', r') ->
      Binop.poly_equal b b'
      && poly_equal l l'
      && poly_equal r r'
    | _ -> false

  let const_int i = Const_int i
  let const_bool b = Const_bool b
  let symbol s = Key s

  let true_ = Const_bool true
  let false_ = Const_bool false

  let is_const (type a) (x : (a, 'k) t) : bool =
    match x with
    | Const_int _ | Const_bool _ -> true
    | Key _ | Not _ | And _ | Binop _ -> false

  let rec binop
    : type a b. (a * a * b) Binop.t -> (a, 'k) t -> (a, 'k) t -> (b, 'k) t
    = fun op x y ->
    match op with
    | Or ->
      begin match x, y with
      | Const_bool true, _ | _, Const_bool true -> Const_bool true
      | Const_bool false, e | e, Const_bool false -> e
      | e1, e2 -> Binop (Or, e1, e2)
      end
    | Equal ->
      begin match x, y with
      | Const_bool true, e -> e
      | e, Const_bool true -> e
      | Const_bool false, e -> not_ e
      | e, Const_bool false -> not_ e
      | Const_int _, Key _ -> Binop (Equal, y, x)
      | Const_int i1, Const_int i2 -> Const_bool (i1 = i2)
      | e1, e2 when equal e1 e2 -> true_
      | e1, e2 -> Binop (Equal, e1, e2)
      end
    | Not_equal -> not_ (binop Equal x y)
    | Plus ->
      begin match x, y with
      | e, Const_int 0
      | Const_int 0, e -> e
      | Const_int i1, Const_int i2 -> Const_int (i1 + i2)
      | e1, e2 -> Binop (Plus, e1, e2)
      end
    | Minus ->
      begin match x, y with
      | e, Const_int 0 -> e
      | Const_int i1, Const_int i2 -> Const_int (i1 - i2)
      | e1, e2 -> Binop (Minus, e1, e2)
      end
    | Times ->
      begin match x, y with
      | e, Const_int 1
      | Const_int 1, e -> e
      | Const_int i1, Const_int i2 -> Const_int (i1 * i2)
      | e1, e2 -> Binop (Times, e1, e2)
      end
    | Divide ->
      begin match x, y with
      | e, Const_int 1 -> e
      | Const_int i1, Const_int i2 -> Const_int (i1 / i2)
      | e1, e2 -> Binop (Divide, e1, e2)
      end
    | Modulus ->
      begin match x, y with
      | Const_int i1, Const_int i2 -> Const_int (i1 mod i2)
      | e1, e2 -> Binop (Modulus, e1, e2)
      end
    | Less_than ->
      begin match x, y with
      | Const_int i1, Const_int i2 -> Const_bool (i1 < i2)
      | e1, e2 -> if equal e1 e2 then false_ else Binop (Less_than, e1, e2)
      end
    | Less_than_eq ->
      begin match x, y with
      | Const_int i1, Const_int i2 -> Const_bool (i1 <= i2)
      | e1, e2 -> if equal e1 e2 then true_ else Binop (Less_than_eq, e1, e2)
      end
    | Greater_than ->
      begin match x, y with
      | Const_int i1, Const_int i2 -> Const_bool (i1 > i2)
      (* Note that we will change greater-than to less-than *)
      | e1, e2 -> if equal e1 e2 then false_ else Binop (Less_than, e2, e1)
      end
    | Greater_than_eq ->
      begin match x, y with
      | Const_int i1, Const_int i2 -> Const_bool (i1 >= i2)
      (* Note that we will change greater-than-eq to less-than-eq *)
      | e1, e2 -> if equal e1 e2 then true_ else Binop (Less_than_eq, e2, e1)
      end

  and not_ (e : (bool, 'k) t) : (bool, 'k) t =
    match e with
    | Const_bool b -> Const_bool (not b)
    | Not e' -> e'
    | Binop (Less_than, e1, e2) ->
      (* not (e1 < e2) = (e2 <= e1) *)
      Binop (Less_than_eq, e2, e1)
    | Binop (Less_than_eq, e1, e2) ->
      (* not (e1 <= e2) = (e2 < e1) *)
      Binop (Less_than, e2, e1)
    | Binop (Or, e1, e2) -> and_ [ not_ e1 ; not_ e2 ] (* it's easier in general to work with "and" *)
    | _ -> Not e

  and and_ (e_ls : (bool, 'k) t list) : (bool, 'k) t =
    match e_ls with
    | [] -> true_ (* vacuous truth *)
    | [ e ] -> e
    | hd :: tl ->
      match hd with
      | Const_bool true -> and_ tl
      | Const_bool false -> false_
      | And e_ls' -> and_ (e_ls' @ tl)
      | e ->
        match and_ tl with
        | Const_bool false -> false_
        | Const_bool true -> e
        | And tl_exprs when List.exists (equal (not_ e)) tl_exprs -> false_
        | And tl_exprs when List.exists (equal e) tl_exprs -> And tl_exprs
        | And tl_exprs -> And (e :: tl_exprs)
        | other when equal other (not_ e) -> false_
        | other when equal other e -> e
        | other -> And [ e ; other ]
end

include T

let transform (type a) (module X : S) (e : (a, 'k) t) : (a, 'k) X.t =
  let rec transform : type a. (a, 'k) t -> (a, 'k) X.t = fun e ->
    match e with
    | Const_int i -> X.const_int i
    | Const_bool b -> X.const_bool b
    | Key s -> X.symbol s
    | Not e' -> X.not_ (transform e')
    | And e_ls -> X.and_ (List.map transform e_ls)
    | Binop (op, e1, e2) -> X.binop op (transform e1) (transform e2)
  in
  transform e

let rec eval
  : type a. default:('c. ('c, 'k) Symbol.t -> 'c) -> 'k Model.t -> (a, 'k) t -> a
  = fun ~default model e ->
  match e with
  | Key s ->
    begin match model.value s with
    | Some a -> a
    | None -> (default s)
    end
  | Const_int i -> i
  | Const_bool b -> b
  | Not e' -> not (eval ~default model e')
  | And e_ls ->
    List.fold_left (fun acc e ->
      acc && eval ~default model e
    ) true e_ls
  | Binop (type b) (op, e1, e2 : (b * b * a) Binop.t * (b, 'k) t * (b, 'k) t) ->
    Binop.to_arithmetic op (eval ~default model e1) (eval ~default model e2)

let default_eval model e =
  eval model e ~default:(fun (type a) (s : (a, 'k) Symbol.t) : a ->
    match s with
    | I _ -> 0
    | B _ -> true
  )

let rec subst
  : type a b. a -> (a, 'k) Symbol.t -> (b, 'k) t  -> (b, 'k) t
  = fun v s e ->
    match e with
    | Key symbol ->
      begin match s, symbol with
      | I k, I k' when Utils.Uid.equal k k' -> const_int v
      | B k, B k' when Utils.Uid.equal k k' -> const_bool v
      | _ -> e
      end
    | Const_int _
    | Const_bool _ -> e
    | Not e' ->
      let e'' = subst v s e' in
      if e' == e'' then
        e
      else
        not_ e''
    | And e_ls ->
      and_ (List.map (subst v s) e_ls)
    | Binop (op, e1, e2) ->
      let e1' = subst v s e1 in
      let e2' = subst v s e2 in
      if e1 == e1' && e2 == e2' then
        e
      else
        binop op e1' e2'

let symbols (type a) (e : (a, 'k) t) : Utils.Uid.Set.t =
  let rec symbols : type a. Utils.Uid.Set.t -> (a, 'k) t -> Utils.Uid.Set.t =
    fun acc e ->
      match e with
      | Const_int _
      | Const_bool _ -> acc
      | Key I uid
      | Key B uid -> Utils.Uid.Set.add uid acc
      | Not e' -> symbols acc e'
      | And e_ls -> List.fold_left symbols acc e_ls
      | Binop (_, e1, e2) -> symbols (symbols acc e1) e2
  in
  symbols Utils.Uid.Set.empty e

module Set = struct
  module Make (K : Symbol.KEY) = struct
    module M = Utils.Set_map.Make_W (struct
      type nonrec t = (bool, K.t) t (* boolean formulas *)
      let compare = compare (* polymorphic comparison is okay *)
    end)

    include M.Set

    (*
      We use SCC for constraint set independence. This ideas originates in
      EXE (https://dl.acm.org/doi/10.1145/1455518.1455522) Section 4.2.
      However, we don't even need to solve the other connect components of
      constraints because we reuse an input environment.
      EXE uses Union Find in practice to do this, though they describe the
      problem with connect components in a graph.

      Since independent constraint sets are solved, there may be repeat
      queries, and it could be beneficial to keep a cache of solved formulas.
      We do not yet do this, though.
    *)
    let scc (formula : (bool, K.t) T.t) ~(wrt : t) : (bool, K.t) T.t =
      let formula_symbols = symbols formula in
      let all_with_symbols =
        list_map (fun e -> (e, symbols e)) wrt
      in
      let rec collect acc_symbols acc_scc remaining =
        let acc_symbols, acc_scc, any_newly_connected, remaining =
          List.fold_left (fun (acc_symbols, acc_scc, any_newly_connected, remaining) (e, e_symbols) ->
            if Utils.Uid.Set.disjoint acc_symbols e_symbols then
              (acc_symbols, acc_scc, any_newly_connected, (e, e_symbols) :: remaining)
            else
              (Utils.Uid.Set.union acc_symbols e_symbols, e :: acc_scc, true, remaining)
            ) (acc_symbols, acc_scc, false, []) remaining
        in
        if any_newly_connected && not (List.is_empty remaining) then
          collect acc_symbols acc_scc remaining
        else
          acc_scc
      in
      and_ @@ collect formula_symbols [ formula ] all_with_symbols
  end
end
