open Utils

type key =
  | Bool_key of Uid.t
  | Int_key of Uid.t

let uid_from_key (key : key) : Uid.t =
  match key with
  | Bool_key k
  | Int_key k -> k

type value =
  | Bool of bool
  | Int of int

(** [Uid.t] Map with additional helpers for working with plain bool and int values *)
module ValueMap = struct
  include Uid.Map
  type binding = key * value
  type map = value Uid.Map.t
  type t = map

  (** [find_int_opt key map] unwraps the int value of KEY in MAP if it exists. *)
  let find_int_opt (key : key) (map : map) : int option =
    match Uid.Map.find_opt key map with
    | Some Int v -> Some v
    | _ -> None

  (** [find_int key map] unwraps the int value of KEY in MAP if it exists or throws *)
  let find_int (key : key) (map : map) : int =
    match find_int_opt key map with
    | Some v -> v
    | _ -> failwith (Printf.sprintf "No int value in MAP for KEY")

  (** [find_bool_opt key map] unwraps the bool value of KEY in MAP if it exists. *)
  let find_bool_opt (key : key) (map : map) : bool option =
    match Uid.Map.find_opt key map with
    | Some Bool v -> Some v
    | _ -> None

  (** [find_bool key map] unwraps the bool value of KEY in MAP if it exists or throws *)
  let find_bool (key : key) (map : map) : bool =
    match find_bool_opt key map with
    | Some v -> v
    | _ -> failwith (Printf.sprintf "No int value in MAP for KEY")

(** [find_symbol_opt key map] returns KEY's uid [value] in MAP *)
  let find_symbol_opt : type a k. (a, k) Symbol.t -> map -> a option = fun key map ->
    match key with
    | B k -> find_bool_opt k map
    | I k -> find_int_opt k map

  (** [add_bool key bool_value map] is the next map with BOOL_VALUE set for KEY *)
  let add_bool (key : key) (bool_value : bool) (map : map) : map =
    add key (Bool bool_value) map

  (** [add_int key int_value map] is the next map with INT_VALUE set for KEY *)
  let add_int (key : key) (int_value : int) (map : map) : map =
    add key (Int int_value) map

  (** [add_int_checked key int_value map] is the next map with INT_VALUE set for KEY
      if KEY has been mapped to INT_VALUE. If it has or the binding doesn't exist,
      then the [Some] next map state is returned otherwise then it returns [None] *)
  let add_int_checked (key : key) (int_value : int) (map : map) : map option =
    match find_int_opt key map with
    | None -> Some (add_int key int_value map)
    | Some old when old = int_value -> Some map
    | Some _ -> None

  (** [add_bool_checked key int_value map] is the next map with BOOL_VALUE set for KEY
      if KEY has been mapped to BOOL_VALUE. If it has or the binding doesn't exist,
      then the [Some] next map state is returned otherwise then it returns [None] *)
  let add_bool_checked key bool_value (map : map) : map option =
    match find_bool_opt key map with
    | None -> Some (add_bool key bool_value map)
    | Some old when Bool.equal old bool_value -> Some map
    | Some _ -> None
end

type 'k t =
  { value : 'a. ('a, 'k) Symbol.t -> 'a option
  ; domain : key list }

let merge (s1 : 'k t) (s2 : 'k t) : 'k t =
  let value (type a) (sym : (a, 'k) Symbol.t) : a option =
    match s1.value sym with
    | None -> s2.value sym
    | v -> v
  in
  { value ; domain = s1.domain @ s2.domain }

let empty : 'k t = { value = (fun _ -> None) ; domain = [] }

let singleton (type a) (a : a) (s : (a, 'k) Symbol.t) : 'k t =
  let value (type b) (s' : (b, 'k) Symbol.t) : b option =
    match s, s' with
    | I uid, I uid' when Uid.equal uid uid' -> Some a
    | B uid, B uid' when Uid.equal uid uid' -> Some a
    | _ -> None
  in
  let single_key =
    match s with
    | (I key) -> Int_key key
    | (B key) -> Bool_key key
  in
  { value ; domain = [single_key] }

(** [from_value_map map] wraps [Uid.t]-keyed MAP with Model.t.

    {[
    module IntMap = Map.Make (Int)
    let () =
      let int_map = (
        IntMap.empty
        |> Map.add_exn ~key:(Char.to_int 'a') ~data:(Int 0)
        |> Map.add_exn ~key:(Char.to_int 'b') ~data:(Int 1)
      ) in
        let pp_model = Model.to_string ~sep:("; ") ~pp_assignment:(
          fun (I x) v -> sprintf " %c => %s" (Char.of_int_exn x) (
            if v = 0 then "hello" else "world"
          )
        ) in
        let model = Model.from_value_map int_map in
        pp_model model [a; b;]
        |> printf "From local: %s\n";
    ]}

    This prints:

    {["From local: { a => hello; b => world }"]} *)
let from_value_map (map : ValueMap.t) : 'k t =
  let domain =
    map
    |> Uid.Map.to_list
    |> List.map (fun (uid, v) ->
        match v with
        | Bool _ -> Bool_key uid
        | Int _ -> Int_key uid)
  in
  { domain
  ; value =
      (fun (type a) (sym : (a, 'k) Symbol.t) : a option ->
        match sym with
        | B key -> (
            match Uid.Map.find_opt key map with
            | Some (Bool b) -> Some b
            | _ -> None)

        | I key -> (
            match Uid.Map.find_opt key map with
            | Some (Int i) -> Some i
            | _ -> None))
  }

let to_string
  (type k)
  (model : k t)
  ~(key : key -> string)
  : string =
  let indent = "  " in
  let entry_to_string : type a. (a, k) Symbol.t -> string -> a -> string =
    fun symbol text v ->
      match symbol with
      | Symbol.B _ ->
        Printf.sprintf "%s\"%s\": %s"
          indent
          text
          (Bool.to_string v)
      | Symbol.I _ ->
        Printf.sprintf "%s\"%s\": %d"
          indent
          text
          v
  in
  let entries =
    model.domain
    |> List.filter_map (fun map_key ->
      match map_key with
      | Bool_key symbol -> (
          let text = key map_key in
          match model.value (B symbol) with
          | None -> None
          | Some v -> Some (entry_to_string (B symbol) text v))
      | Int_key symbol -> (
          let text = key map_key in
          match model.value (I symbol) with
          | None -> None
          | Some v -> Some (entry_to_string (I symbol) text v)))
  in

  match entries with
  | [] -> "{\n}"
  | _ -> "{\n" ^ String.concat ",\n" entries ^ "\n}"

let ascii_key = function
  | Bool_key x
  | Int_key x -> Symbol.AsciiSymbol.to_string x

let prefix_key =
  function
  | Bool_key x -> "B" ^ (Int.to_string @@ Uid.to_int x)
  | Int_key x -> "I" ^ (Int.to_string @@ Uid.to_int x)
