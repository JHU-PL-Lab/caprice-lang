
type iii = int * int * int
type iib = int * int * bool
type bbb = bool * bool * bool

type occurs = private Occurs (* occurs in formulas *)
type constr = private Constr (* only used to construct formulas *)

type (_, _) c =
  (* occurs in formulas *)
  | Plus : (iii, occurs) c
  | Minus : (iii, occurs) c
  | Times : (iii, occurs) c
  | Divide : (iii, occurs) c
  | Modulus : (iii, occurs) c
  | Less_than : (iib, occurs) c
  | Less_than_eq : (iib, occurs) c
  | Equal : ('a * 'a * bool, occurs) c
  | Or : (bbb, occurs) c
  | Iff : (bbb, occurs) c
  (* construct only *)
  | Greater_than : (iib, constr) c
  | Greater_than_eq : (iib, constr) c
  | Not_equal : ('a * 'a * bool, constr) c

type 'a t = ('a, occurs) c

let poly_equal (type a b) (x : a t) (y : b t) : bool =
  match x, y with
  | Plus, Plus
  | Minus, Minus
  | Times, Times
  | Divide, Divide
  | Modulus, Modulus
  | Less_than, Less_than
  | Less_than_eq, Less_than_eq
  | Equal, Equal -> true
  | Iff, Iff -> true
  | _ -> false

let to_arithmetic (type a b) (binop : (a * a * b) t) : a -> a -> b =
  match binop with
  | Plus -> ( + )
  | Minus -> ( - )
  | Times -> ( * )
  | Divide -> ( / )
  | Modulus -> ( mod )
  | Less_than -> ( < )
  | Less_than_eq -> ( <= )
  | Equal -> ( = ) (* polymorphic equality *)
  | Or -> ( || )
  | Iff -> Bool.equal

let to_string (type a b) (binop : (a * a * b) t) : string =
  match binop with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Modulus -> "%"
  | Less_than -> "<"
  | Less_than_eq -> "<="
  | Equal -> "="
  | Or -> "||"
  | Iff -> "<=>"
