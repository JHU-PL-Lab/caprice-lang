%{
  open Lang
  open Ast
  open Binop
  open Tools
%}

%parameter<Param : Param.S>

/*
 * Precedences and associativities.  Lower precedences come first.
 */
%nonassoc prec_let prec_fun   /* Let-ins and functions */
%nonassoc prec_if             /* Conditionals */
%nonassoc prec_mu             /* mu types */
%nonassoc OF                  /* variant type declarations */
%nonassoc AS                  /* pattern as ident */
%right PIPE                   /* multiple patterns, variant type separator */
%left COMMA                   /* tuples */
%right DOUBLE_PIPE            /* || for boolean or */
%right DOUBLE_AMPERSAND       /* && for boolean and */
%right NOT                    /* Not */
/* == <> < <= > >= */
%left EQUAL_EQUAL NOT_EQUAL LESS LESS_EQUAL GREATER GREATER_EQUAL
%right DOUBLE_COLON           /* :: */
%right prec_variant_pattern   /* variant destruction pattern */
%left PLUS MINUS              /* + - */
%right ARROW                  /* -> for type declaration */
%left ASTERISK SLASH PERCENT  /* * / % */

(* HACK: Precedence declarations to resolve (type a) -> t parsing.
   When parsing "(type" followed by identifier, we want to shift the identifier
   to eventually parse the type parameter sugar (type a) -> t, rather than
   reduce TYPE to expr early (which would parse as function application).
   The higher precedence on IDENTIFIER causes Menhir to prefer shifting. *)
%nonassoc TYPE
%nonassoc IDENTIFIER

%start <statement list> prog
%start <statement_with_pos list> prog_with_pos

%%

prog:
  | statement+ EOF
    { $1 }
  ;

prog_with_pos:
  | statement_with_pos+ EOF
    { $1 }
  ;

statement:
  | LET b=binding EQUALS defn=expr
    { SLet { name = fst b ; annot = snd b ; defn } }
  | LET name=l_ident params=l_ident+ EQUALS body=expr
    { SLet { name ; annot = ANone ; defn = mk_curried_fun params body } }
  | LET name=l_ident tparams=typed_params COLON body_type=expr EQUALS body=expr
    { SLet { name ; annot = AType { tau = (mk_curried_funtype tparams body_type) ; do_check = true }
      ; defn = mk_curried_fun (extract_param_names tparams) body } }
  | LET REC name=l_ident param=l_ident params=l_ident* EQUALS body=expr
    { SLetRec { name ; annot = ANone ; param ; defn = mk_curried_fun params body } }
  | LET REC name=l_ident tparams=typed_params COLON body_type=expr EQUALS body=expr
    { SLetRec { name
      ; annot = AType { tau = (mk_curried_funtype tparams body_type) ; do_check = true }
      ; param = fst (List.hd tparams)
      ; defn = mk_curried_fun (List.tl (extract_param_names tparams)) body } }
  | LET REC b=binding EQUALS FUNCTION param=l_ident params=l_ident* ARROW body=expr
    { SLetRec { name = fst b ; annot = snd b ; param ; defn = mk_curried_fun params body } }
  ;

statement_with_pos:
  | s=statement
    { (s, { begins = $startpos ; ends = $endpos } ) }

%inline binding:
  | name=l_ident COLON tau=expr
  | OPEN_PAREN name=l_ident COLON tau=expr CLOSE_PAREN
    { name, AType { tau ; do_check = true } }
  | name=l_ident
    { name, ANone }
  ;

typed_params:
  | typed_param_group+
    { List.concat $1 }
  ;

typed_param_group:
  | OPEN_PAREN tp=typed_param CLOSE_PAREN
    { [ tp ] }
  | OPEN_PAREN TYPE type_params=ttype_param+ CLOSE_PAREN
    { type_params }
  ;

%inline typed_name:
  | name=l_ident COLON tau=expr
    { name, tau }
  | name=l_ident COLON tau=expr PIPE predicate=expr
    { let t = Param.make_refinement name ~tau ~predicate { begins = $startpos ; ends = $endpos } in
      name, t }
  | name=l_ident COLON_EQUAL e=expr
    { name, ETypeSingle e }

%inline typed_param:
  | p=typed_name
    { fst p, (None, snd p) }
  | DEP p=typed_name
  | DEPENDENT p=typed_name
    { fst p, (Some (fst p), snd p) }
  ;

%inline ttype_param:
  | type_id=ident
    { type_id, (Some type_id, EType) }
  ;

expr:
  | appl_expr /* Includes primary expressions */
  | op_expr
  | type_expr
    { $1 }
  | left=expr COMMA right=expr
    { ETuple (left, right) }
  | IF if_=expr THEN then_=expr ELSE else_=expr %prec prec_if
    { EIf { if_ ; then_ ; else_ } }
  | FUNCTION params=l_ident+ ARROW body=expr %prec prec_fun
    { mk_curried_fun params body }
  | stmt=statement IN body=expr %prec prec_let
    { ELet { stmt ; body } }
  | MATCH subject=expr WITH ioption(PIPE) patterns=match_expr_list END
    { EMatch { subject ; patterns } }
  ;

%inline type_expr:
  | ioption(PIPE) v_type=variant_type_body
    { ETypeVariant v_type }
  | MU var=l_ident DOT body=expr %prec prec_mu
    { ETypeMu { var ; body } }
  | function_type
    { $1 }
  ;

%inline function_type:
  (* regular function *)
  | tdom=expr ARROW codomain=expr
    { ETypeFun { domain = None, tdom ; codomain } }
  (* standard dependent function type *)
  | OPEN_PAREN pair=typed_name CLOSE_PAREN ARROW codomain=expr
    { ETypeFun { domain = Some (fst pair), snd pair ; codomain } }
  | OPEN_PAREN TYPE type_ids=ident+ CLOSE_PAREN ARROW codomain=expr
    { List.fold_right (fun type_id acc ->
      ETypeFun { domain = Some type_id, EType ; codomain = acc }
      ) type_ids codomain }
  ;

variant_type_body:
  | label=variant_label OF payload=expr
    { [ { label ; payload } ] }
  | label=variant_label OF payload=expr PIPE rest=variant_type_body
    { { Variant.label ; payload } :: rest }

appl_expr:
  | func=appl_expr arg=primary_expr
    { EAppl { func ; arg } }
  | label=variant_label payload=primary_expr
    { EVariant { label ; payload } }
  | ASSERT cond=primary_expr
    { EAssert cond }
  | ASSUME cond=primary_expr
    { EAssume cond }
  | primary_expr
    { $1 }
  ;

/* In a primary_expr, only primitives, vars, records, and lists do not need
   surrounding parentheses. */
primary_expr:
  | INT
    { EInt $1 }
  | BOOL
    { EBool $1 }
  | ident_usage
    { $1 }
  | TYPE
    { EType }
  | INT_KEYWORD
    { ETypeInt }
  | BOOL_KEYWORD
    { ETypeBool }
  | UNIT_KEYWORD
    { ETypeUnit }
  | TOP_KEYWORD
    { ETypeTop }
  | BOTTOM_KEYWORD
    { ETypeBottom }
  | LIST
    { EFunction { param = Ident "~list" ; body = ETypeList (EVar (Ident "~list")) } } (* HACK HACK HACK *)
  | ABSTRACT
    { EAbstractType }
  | SINGLETON
    { EFunction { param = Ident "~singleton" ; body = ETypeSingle (EVar (Ident "~singleton")) } } (* HACK HACK HACK *)
  | OPEN_PAREN CLOSE_PAREN
    { EUnit }
  | OPEN_BRACE COLON CLOSE_BRACE
    { ETypeRecord Record.empty }
  | OPEN_BRACE record_body CLOSE_BRACE
    { ERecord $2 }
  | OPEN_BRACE CLOSE_BRACE
    { ERecord Record.empty }
  | OPEN_BRACKET list_items=separated_list(SEMICOLON, expr) CLOSE_BRACKET
    { List.fold_right (fun hd tl ->
        EListCons { hd ; tl }
      ) list_items EEmptyList }
  | OPEN_PAREN e=expr CLOSE_PAREN
    { e }
  | STRUCT stmts=statement* END (* may be empty *)
    { EModule stmts }
  | SIG val_decls=val_decl* END
    { ETypeModule val_decls }
  | record_type_or_refinement
    { $1 }
  | record=primary_expr DOT label=record_label
    { EProject { record ; label } }
  ;

op_expr:
  | left=expr ASTERISK right=expr
    { EBinop { left ; binop = BTimes ; right } }
  | left=expr SLASH right=expr
    { EBinop { left ; binop = BDivide ; right } }
  | left=expr PERCENT right=expr
    { EBinop { left ; binop = BModulus ; right } }
  | left=expr PLUS right=expr
    { EBinop { left ; binop = BPlus ; right } }
  | left=expr MINUS right=expr
    { EBinop { left ; binop = BMinus ; right } }
  | hd=expr DOUBLE_COLON tl=expr
    { EListCons { hd ; tl } }
  | left=expr EQUAL_EQUAL right=expr
    { EBinop { left ; binop = BEqual ; right } }
  | left=expr NOT_EQUAL right=expr
    { EBinop { left ; binop = BNeq ; right } }
  | left=expr GREATER right=expr
    { EBinop { left ; binop = BGreaterThan ; right } }
  | left=expr GREATER_EQUAL right=expr
    { EBinop { left ; binop = BGeq ; right } }
  | left=expr LESS right=expr
    { EBinop { left ; binop = BLessThan ; right } }
  | left=expr LESS_EQUAL right=expr
    { EBinop { left ; binop = BLeq ; right } }
  | NOT body=expr
    { ENot body }
  | left=expr DOUBLE_AMPERSAND right=expr
    { EBinop { left ; binop = BAnd ; right } }
  | left=expr DOUBLE_PIPE right=expr
    { EBinop { left ; binop = BOr ; right } }
  | MINUS i=INT
    { EInt (-i) }
  ;

%inline record_type_or_refinement:
  (* exactly one label *)
  | OPEN_BRACE record=record_type_body CLOSE_BRACE
    { ETypeRecord record }
  (* refinement type with binding for tau, which looks like a record type at first *)
  | OPEN_BRACE var=l_ident COLON tau=expr PIPE predicate=expr CLOSE_BRACE
    { Param.make_refinement var ~tau ~predicate { begins = $startpos ; ends = $endpos } }
  ;

%inline record_type_item:
  | label=record_label COLON tau=expr
    { label, tau }
  | label=record_label COLON_EQUAL e=expr
    { label, ETypeSingle e }
  ;

%inline record_expr_item:
  | label=record_label EQUALS e=expr
    { label, e }
  | label=record_label (* punning *)
    { label, EVar (Record.Label.to_ident label) }
  ;

record_type_body:
  | items=separated_nonempty_list(SEMICOLON, record_type_item)
    { Record.Parsing.of_list items }
  ;

%inline record_label:
  | ident
    { Record.Label.RecordLabel $1 }
  ;

%inline ident_usage:
  | ident
    { EVar $1 }
  ;

%inline l_ident: (* like "lvalue". These are idents that can be assigned to *)
  | ident
    { $1 }
  | UNDERSCORE
    { Ident.Ident "_" }
  ;

%inline ident: (* these are idents that can be used as values *)
  | IDENTIFIER
    { Ident.Ident $1 }
  ;

%inline val_decl:
  | VAL pair=record_type_item
    { pair }

/* **** Records, lists, and variants **** */

/* e.g. { x = 1 ; y = 2 ; z = 3 } */
record_body:
  | items=separated_nonempty_list(SEMICOLON, record_expr_item)
    { Record.Parsing.of_list items }
  ;

/* e.g. `Variant 0 */
variant_label:
  | BACKTICK label=ident
    { Variant.Label.VariantLabel label }
  ;

/* **** Pattern matching **** */

match_expr_list:
  | separated_nonempty_list(PIPE, pat=pattern ARROW body=expr { pat, body })
    { $1 }

pattern:
  | pat=pattern AS name=ident
    { PPatternAs (pat, name) }
  | hd=pattern DOUBLE_COLON tl=pattern
    { PDestructList (hd, tl) }
  | label=variant_label payload=pattern %prec prec_variant_pattern
    { PVariant { Variant.label ; payload } }
  | left=pattern COMMA right=pattern
    { PTuple (left, right)}
  | OPEN_PAREN CLOSE_PAREN
    { PUnit }
  | OPEN_BRACKET CLOSE_BRACKET
    { PEmptyList }
  | UNDERSCORE
    { PAny }
  | hd_pat=pattern PIPE rest=pattern
    { match rest with (* since pipe is right assoc, the hd_pat is not an "or" pattern *)
      | Pattern.PPatternOr p_ls -> PPatternOr (hd_pat :: p_ls)
      | p -> PPatternOr [ hd_pat ; p ]
    }
  | ident
    { Pattern.PVariable $1 } (* not l_ident because we handle underscore immediately above *)
  | OPEN_PAREN pat=pattern CLOSE_PAREN
    { pat }
  ;
