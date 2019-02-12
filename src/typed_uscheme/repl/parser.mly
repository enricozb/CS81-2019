%{
  let loc_of_token = function
    | TYPESTR (l, _)
    | NAME (l, _)
    | NUMBER (l, _)
    | IF l
    | ELSE l
    | DEF l
    | COLON l
    | EQUALS l
    | MAPSTO l
    | COMMA l
    | QUOTE l
    | LANGLE l | RANGLE l
    | LBRACE l | RBRACE l
    | LPAREN l | RPAREN l
    | LBRACK l | RBRACK l
    | OPERATOR (l, _)
    | INDENT l | DEDENT l | NEWLINE l
    | DEINDENT (l, _)
      -> l
    | EOF -> failwith "EOF has no `loc`"
%}

%token <Loc.loc * string> TYPESTR
%token <Loc.loc * string> NAME
%token <Loc.loc * int> NUMBER
%token <Loc.loc> IF ELSE
%token <Loc.loc> DEF
%token <Loc.loc> COLON
%token <Loc.loc> EQUALS MAPSTO
%token <Loc.loc> COMMA QUOTE
%token <Loc.loc> LANGLE RANGLE
%token <Loc.loc> LBRACE RBRACE
%token <Loc.loc> LPAREN RPAREN
%token <Loc.loc> LBRACK RBRACK
%token <Loc.loc * string> OPERATOR
%token <Loc.loc> INDENT DEDENT NEWLINE
%token <Loc.loc * int> DEINDENT
%token EOF
%start main             /* the entry point */
%type <Ast.ast> main

%%

main:
  | NEWLINE main            { $2 }
  | simple_stmt             { $1 }
(* for interactive input NEWLINE must be after compound_stmt *)
  | compound_stmt NEWLINE   { $1 }

stmt:
  | simple_stmt             { $1 }
  | compound_stmt           { $1 }

simple_stmt:
  | expr NEWLINE            { $1 }
  | assign_stmt NEWLINE     { $1 }

suite:
  | simple_stmt                     { [$1] }
  | NEWLINE INDENT stmt_list DEDENT { $3 }

stmt_list:
  | stmt                    { [$1] }
  | stmt stmt_list          { $1 :: $2 }

assign_stmt:
  | NAME EQUALS expr {
    Ast.Bind (Loc.span (fst $1) (Ast.loc_of_ast $3), snd $1, $3)
  }

compound_stmt:
  | if_stmt                 { $1 }
  | funcdef                 { $1 }

(* TODO disallow stuff like `if True: x else: y` in one line *)
if_stmt:
  | IF expr COLON suite ELSE COLON suite {
    Ast.If (Loc.span $1 (Ast.loc_of_ast_list $7),
            $2, $4, $7)
  }

funcdef:
  | DEF NAME generic_list typed_namelist MAPSTO ty COLON suite {
    Ast.Def (Loc.span $1 (Ast.loc_of_ast_list $8),
             snd $2,
                 $3,
             snd $4,
             $6,
             $8)
  }
  | DEF NAME typed_namelist MAPSTO ty COLON suite {
    Ast.Def (Loc.span $1 (Ast.loc_of_ast_list $7),
             snd $2,
                 [],
             snd $3,
             $5,
             $7)
  }

generic_list:
  | LANGLE RANGLE                        { [] }
  | LANGLE generic_list_inner RANGLE     { $2 }

generic_list_inner:
  | typevar                                 { [$1] }
  | typevar COMMA generic_list_inner        { $1 :: $3 }

typed_namelist:
  | LPAREN RPAREN                        { (Loc.span $1 $2, []) }
  | LPAREN typed_namelist_inner RPAREN   { (Loc.span $1 $3, $2) }

typed_namelist_inner:
  | NAME COLON ty                            { [(snd $1, $3)] }
  | NAME COLON ty COMMA typed_namelist_inner { (snd $1, $3) :: $5 }

expr:
  | literal                 { $1 }
  | NAME                    { Ast.Name (fst $1, snd $1) }
  | lambda                  { $1 }
  | call                    { $1 }
  | LPAREN expr RPAREN      { $2 }
  | operator_list           { $1 }

literal:
  | NUMBER       { Ast.Num (fst $1, snd $1) }

lambda:
  | typed_namelist MAPSTO expr {
    Ast.Lambda (Loc.span (fst $1) (Ast.loc_of_ast $3), snd $1, $3)
  }

call:
  | NAME LPAREN expr_list_inner RPAREN {
    Ast.Call (Loc.span (fst $1) $4, snd $1, $3)
  }
  | NAME LANGLE type_list_inner RANGLE LPAREN expr_list_inner RPAREN {
    Ast.InstantiatedCall (Loc.span (fst $1) $7, snd $1, $3, $6)
  }

operator_list:
  | expr LANGLE expr        {
    Ast.Call (Loc.span (Ast.loc_of_ast $1) (Ast.loc_of_ast $3),
              "<", [$1; $3])
  }
  | expr RANGLE expr {
    Ast.Call (Loc.span (Ast.loc_of_ast $1) (Ast.loc_of_ast $3),
              ">", [$1; $3])
  }
  | expr OPERATOR expr      {
    Ast.Call (Loc.span (Ast.loc_of_ast $1) (Ast.loc_of_ast $3),
              snd $2, [$1; $3])
  }

expr_list_inner:
  | { [] }
  | expr                       { [$1] }
  | expr COMMA expr_list_inner { ($1 :: $3) }

(* Types *)
ty:
  | simple_ty              { $1 }
  | complex_ty             { $1 }

typevar:
  | QUOTE TYPESTR          { snd $2 }

simple_ty:
  | typevar                               { Ast.TyVar $1 }
  | TYPESTR                               { Ast.TyStr (snd $1) }
  | TYPESTR LANGLE type_list_inner RANGLE { Ast.TyApp (snd $1, $3) }

simple_ty_list:
  | simple_ty                             { [$1] }
  | simple_ty simple_ty_list              { $1 :: $2 }

complex_ty:
  | LPAREN simple_ty_list MAPSTO ty RPAREN  { Ast.TyFun ($2, $4) }

type_list_inner:
  | ty                                      { [$1] }
  | ty COMMA type_list_inner                { $1 :: $3 }

