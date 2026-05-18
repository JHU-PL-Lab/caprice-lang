module IntSet = Utils.Iterables.IntSet
open Smt

let stringify x = x |> Char.chr |> String.of_char

type token_kind =
  | ID of string
  | INT of int
  | NOT
  | AND
  | OR
  | LP
  | RP
  | CMP of string (* = < > <= >= != *)
  | ARITH of char (* + - * % *)
  | EOF

type token = { kind : token_kind; pos : int }

exception Lex_error of int * string
exception Parse_error of int * string

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let is_alnum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let tokenize (s : string) : token list =
  let n = String.length s in
  let peek i = if i < n then Some s.[i] else None in
  let rec loop i acc =
    if i >= n then List.rev ({ kind = EOF; pos = i } :: acc)
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' -> loop (i + 1) acc
      | '(' -> loop (i + 1) ({ kind = LP; pos = i } :: acc)
      | ')' -> loop (i + 1) ({ kind = RP; pos = i } :: acc)
      | '^' -> loop (i + 1) ({ kind = AND; pos = i } :: acc)
      | '|' -> loop (i + 1) ({ kind = OR; pos = i } :: acc)
      | ('+' | '-' | '*' | '%' | '/') as c ->
          (* Could be negative int if '-' followed by digit *)
          begin match (c, peek (i + 1)) with
          | '-', Some d when is_digit d ->
              (* lex a negative int literal *)
              let j = ref (i + 1) in
              while !j < n && is_digit s.[!j] do
                incr j
              done;
              let txt = String.sub s i (!j - i) in
              let v =
                try int_of_string txt
                with _ -> raise (Lex_error (i, "bad integer literal"))
              in
              loop !j ({ kind = INT v; pos = i } :: acc)
          | _ -> loop (i + 1) ({ kind = ARITH c; pos = i } :: acc)
          end
      | ('<' | '>' | '!' | '=') as c ->
          (* comparisons: <= >= != or single = < > *)
          let two =
            match peek (i + 1) with
            | Some '=' -> Some (String.make 1 c ^ "=")
            | _ -> None
          in
          begin match (c, two) with
          | ('<' | '>' | '!'), Some op ->
              loop (i + 2) ({ kind = CMP op; pos = i } :: acc)
          | '=', Some _ ->
              (* "==" is not in grammar; treat first '=' as '=' and leave next '='? better error *)
              raise (Lex_error (i, "unexpected '==' (use '=')"))
          | '!', None ->
              raise (Lex_error (i, "unexpected '!' (did you mean '!='?)"))
          | ('<' | '>' | '='), None ->
              loop (i + 1) ({ kind = CMP (String.make 1 c); pos = i } :: acc)
          | _ -> assert false
          end
      | c when is_digit c ->
          let j = ref i in
          while !j < n && is_digit s.[!j] do
            incr j
          done;
          let txt = String.sub s i (!j - i) in
          let v =
            try int_of_string txt
            with _ -> raise (Lex_error (i, "bad integer literal"))
          in
          loop !j ({ kind = INT v; pos = i } :: acc)
      | c when is_alpha c ->
          let j = ref (i + 1) in
          while !j < n && is_alnum s.[!j] do
            incr j
          done;
          let word = String.sub s i (!j - i) in
          if word = "not" then loop !j ({ kind = NOT; pos = i } :: acc)
          else loop !j ({ kind = ID word; pos = i } :: acc)
      | c -> raise (Lex_error (i, Printf.sprintf "unexpected character %C" c))
  in
  loop 0 []

type parser = { toks : token array; mutable i : int }

let cur p =
  if p.i < Array.length p.toks then p.toks.(p.i)
  else { kind = EOF; pos = p.toks.(Array.length p.toks - 1).pos }

let advance p = p.i <- p.i + 1

let expect p = function
  | LP ->
      begin match (cur p).kind with
      | LP -> advance p
      | _ -> raise (Parse_error ((cur p).pos, "expected '('"))
      end
  | RP ->
      begin match (cur p).kind with
      | RP -> advance p
      | _ -> raise (Parse_error ((cur p).pos, "expected ')'"))
      end
  | _ -> invalid_arg "expect: only LP/RP supported"

let match_kind p f =
  match (cur p).kind with
  | k when f k ->
      let t = cur p in
      advance p;
      Some t
  | _ -> None

let fold_or = function
  | [] -> invalid_arg "fold_or: empty"
  | x :: xs -> List.fold_left (fun acc e -> Formula.binop Binop.Or acc e) x xs

let binop_of_cmp = function
  | "=" -> Binop.Equal
  | "<" -> Binop.Less_than
  | "<=" -> Binop.Less_than_eq
  | s -> raise (Failure ("unknown comparison operator: " ^ s))

let rec paren_contains_cmp toks i depth =
  if i >= Array.length toks then false
  else
    match toks.(i).kind with
    | LP -> paren_contains_cmp toks (i + 1) (depth + 1)
    | RP ->
        if depth = 1 then false else paren_contains_cmp toks (i + 1) (depth - 1)
    | CMP _ when depth = 1 -> true
    | _ -> paren_contains_cmp toks (i + 1) depth

let rec find_matching_rp toks i depth =
  if i >= Array.length toks then None
  else
    match toks.(i).kind with
    | LP -> find_matching_rp toks (i + 1) (depth + 1)
    | RP ->
        if depth = 1 then Some i else find_matching_rp toks (i + 1) (depth - 1)
    | _ -> find_matching_rp toks (i + 1) depth

(* Forward decls *)
let rec parse_or (p : parser) : (bool, 'k) Formula.t =
  let left = parse_and p in
  let rec gather acc =
    match match_kind p (function OR -> true | _ -> false) with
    | None -> fold_or (List.rev acc)
    | Some _ ->
        let rhs = parse_and p in
        gather (rhs :: acc)
  in
  gather [ left ]

and parse_and (p : parser) : (bool, 'k) Formula.t =
  let left = parse_not p in
  let rec gather acc =
    match match_kind p (function AND -> true | _ -> false) with
    | None ->
        begin match List.rev acc with
        | [ x ] -> x (* 🔴 CRITICAL FIX *)
        | xs -> Formula.and_ xs
        end
    | Some _ ->
        let rhs = parse_not p in
        gather (rhs :: acc)
  in
  gather [ left ]

and parse_not (p : parser) : (bool, 'k) Formula.t =
  match match_kind p (function NOT -> true | _ -> false) with
  | Some _ -> Formula.not_ (parse_not p)
  | None -> parse_bool_primary p

and parse_compare (p : parser) : (bool, 'k) Formula.t =
  let left = parse_add p in
  match match_kind p (function CMP _ -> true | _ -> false) with
  | Some t ->
      let op =
        match t.kind with
        | CMP "=" -> Binop.Equal
        | CMP "<" -> Binop.Less_than
        | CMP "<=" -> Binop.Less_than_eq
        | _ -> assert false
      in
      let right = parse_add p in
      Formula.binop op left right
  | None -> raise (Parse_error ((cur p).pos, "expected comparison operator"))

and parse_add (p : parser) : (int, 'k) Formula.t =
  let node = ref (parse_mul p) in
  let rec loop () =
    match (cur p).kind with
    | ARITH '+' ->
        advance p;
        let rhs = parse_mul p in
        node := Formula.binop Plus !node rhs;
        loop ()
    | ARITH '-' ->
        advance p;
        let rhs = parse_mul p in
        node := Formula.binop Minus !node rhs;
        loop ()
    | _ -> !node
  in
  loop ()

and parse_mul (p : parser) : (int, 'k) Formula.t =
  let node = ref (parse_unary p) in
  let rec loop () =
    match (cur p).kind with
    | ARITH '*' ->
        advance p;
        let rhs = parse_unary p in
        node := Formula.binop Times !node rhs;
        loop ()
    | ARITH '%' ->
        advance p;
        let rhs = parse_unary p in
        node := Formula.binop Modulus !node rhs;
        loop ()
    | ARITH '/' ->
        advance p;
        let rhs = parse_unary p in
        node := Formula.binop Divide !node rhs;
        loop ()
    | _ -> !node
  in
  loop ()

and parse_unary (p : parser) : (int, 'k) Formula.t =
  match (cur p).kind with
  | ARITH '-' ->
      advance p;
      let e = parse_unary p in
      Formula.binop Minus (Formula.const_int 0) e
  | _ -> parse_primary p

and parse_bool_primary (p : parser) : (bool, 'k) Formula.t =
  match (cur p).kind with
  | LP ->
      begin match find_matching_rp p.toks p.i 0 with
      | Some j when j + 1 < Array.length p.toks ->
          begin match p.toks.(j + 1).kind with
          | CMP _ ->
              (* This '(' is the left operand of a comparison *)
              parse_compare p
          | _ ->
              (* This is a true boolean parenthesis *)
              advance p;
              let e = parse_or p in
              expect p RP;
              e
          end
      | _ ->
          (* Fallback: boolean parentheses *)
          advance p;
          let e = parse_or p in
          expect p RP;
          e
      end
  | ID s ->
      begin match p.toks.(p.i + 1).kind with
      | CMP _ -> parse_compare p
      | _ ->
          advance p;
          let ch, _ =
            Option.value ~default:('@', Seq.empty)
              (Seq.uncons (String.to_seq s))
          in
          let sym = Symbol.AsciiSymbol.make_bool ch in
          Formula.symbol sym
      end
  | INT _ ->
      (* Comparisons like (0 = a) *)
      parse_compare p
  | _ -> raise (Parse_error ((cur p).pos, "expected boolean expression"))

and parse_primary p =
  match (cur p).kind with
  | LP ->
      advance p;
      let e = parse_add p in
      expect p RP;
      e
  | ID s ->
      advance p;
      let ch, _ =
        Option.value ~default:('@', Seq.empty) (Seq.uncons (String.to_seq s))
      in
      let sym = Symbol.AsciiSymbol.make_int ch in
      Formula.symbol sym
  | INT n ->
      advance p;
      Formula.const_int n
  | EOF -> raise (Parse_error ((cur p).pos, "unexpected end of input"))
  | _ -> raise (Parse_error ((cur p).pos, "expected primary"))

let parse (s : string) : (bool, 'k) Formula.t =
  let toks = Array.of_list (tokenize s) in
  let p = { toks; i = 0 } in
  let e = parse_or p in
  begin match (cur p).kind with
  | EOF -> e
  | _ -> raise (Parse_error ((cur p).pos, "trailing input"))
  end

let from_stdin () : string list =
  let lines =
    let rec loop acc =
      match input_line stdin with
      | line -> loop (line :: acc)
      | exception End_of_file -> List.rev acc
    in
    loop []
  in
  let flush buf acc =
    match buf with
    | [] -> acc
    | _ ->
        let joined =
          buf |> List.rev
          |> List.filter (fun s -> String.trim s <> "")
          |> List.map String.trim |> String.concat " "
        in
        if joined = "" then acc else joined :: acc
  in
  let rec go buf acc = function
    | [] -> List.rev (flush buf acc)
    | line :: tl ->
        if String.trim line = "" then go [] (flush buf acc) tl
        else go (line :: buf) acc tl
  in
  go [] [] lines
