
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
  | EPick_i
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
  | AType of { typ : t ; do_check : bool }

and statement =
  | SLet of { name : Ident.t ; annot : annot ; defn : t }
  | SLetRec of { name : Ident.t ; annot : annot ; param : Ident.t ; defn : t }

type program = statement list

(* full position, and then a smaller position that contains a representative
  portion of the statement to be used in a warning, for example. *)
type pos = { full : Utils.Pos.Span.t ; small : Utils.Pos.Span.t }

type program_with_pos = (statement * pos) list

let id_of_stmt = function
  | SLet { name ; _ }
  | SLetRec { name ; _ } -> name
