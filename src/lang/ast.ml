
type t =
  | EUnit
  | EInt of int
  | EBool of bool
  | EVar of Ident.t
  | EBinop of { left : t ; binop : Binop.t ; right : t }
  | EIf of { if_ : t ; then_ : t ; else_ : t }
  | ELet of { stmt : statement ; body : t }
  | EAppl of { func : t ; arg : t }
  | EMatch of { subject : t ; patterns : (Pattern.t * t) list }
  | EProject of { record : t ; label : Record.Label.t }
  | ERecord of t Record.t
  | ETuple of t * t
  | EEmptyList
  | EListCons of { hd : t ; tl : t }
  | EModule of statement list
  | ENot of t
  | EFunction of { param : Ident.t ; body : t }
  | EVariant of t Variant.t
  | EAssert of t
  | EAssume of t
  | EAbstractType (* evaluates to an abstract type *)
  (* Types *)
  | EType
  | ETypeInt
  | ETypeBool
  | ETypeTop
  | ETypeBottom
  | ETypeUnit
  | ETypeRecord of t Record.t
  | ETypeModule of (Record.Label.t * t) list
  | ETypeFun of (Ident.t option * t, t) Funtype.t
  | ETypeRefine of (t, t) Refinement.t
  | ETypeMu of { var : Ident.t ; body : t }
  | ETypeList of t
  | ETypeVariant of t Variant.t list
  | ETypeSingle of t

and annot =
  | ANone
  | AType of { tau : t ; do_check : bool }

and statement =
  | SLet of { name : Ident.t ; annot : annot ; defn : t }
  | SLetRec of { name : Ident.t ; annot : annot ; param : Ident.t ; defn : t }

type pos_span = { begins : Lexing.position ; ends : Lexing.position }

type statement_with_pos = statement * pos_span

type program = statement list

type program_with_pos = statement_with_pos list

let compare_pos_span a b =
  match Int.compare a.begins.pos_cnum b.begins.pos_cnum with
  | 0 -> Int.compare a.ends.pos_cnum b.ends.pos_cnum
  | cmp -> cmp

let equal_pos_span a b = compare_pos_span a b = 0

let id_of_stmt = function
  | SLet { name ; _ }
  | SLetRec { name ; _ } -> name
