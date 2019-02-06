%{
  let loc_of_token = function
    | TYPE (l, _)
    | NAME (l, _)
    | NUMBER (l, _)
    | IF l
    | DEF l
    | COLON l
    | EQUALS l
    | MAPSTO l
    | COMMA l
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

%token <Loc.loc * string> TYPE
%token <Loc.loc * string> NAME
%token <Loc.loc * int> NUMBER
%token <Loc.loc> IF
%token <Loc.loc> DEF
%token <Loc.loc> COLON
%token <Loc.loc> EQUALS MAPSTO
%token <Loc.loc> COMMA
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
  | stmt                    { $1 }

stmt:
  | simple_stmt             { $1 }
  | compound_stmt NEWLINE   { $1 }

simple_stmt:
  | expr NEWLINE            { $1 }
  | assign_stmt NEWLINE     { $1 }

assign_stmt:
  | NAME EQUALS expr {
    Ast.Bind (Loc.span (fst $1) (Ast.loc_of_ast $3), snd $1, $3)
  }

compound_stmt:
  | funcdef                 { $1 }

funcdef:
  | DEF NAME generic_list typed_namelist MAPSTO TYPE COLON suite {
    Ast.Def (Loc.span $1 (Ast.loc_of_ast_list $8), snd $2, snd $4, snd $6, $8)
  }

generic_list:
  | LANGLE TYPE RANGLE       { [$2] }

typed_namelist:
  | LPAREN RPAREN                        { (Loc.span $1 $2, []) }
  | LPAREN typed_namelist_inner RPAREN   { (Loc.span $1 $3, $2) }

typed_namelist_inner:
  | NAME COLON TYPE                            { [(snd $1, snd $3)] }
  | NAME COLON TYPE COMMA typed_namelist_inner { (snd $1, snd $3) :: $5 }

suite:
  | simple_stmt             { [$1] }
  | NEWLINE INDENT stmt_list DEDENT { $3 }

stmt_list:
  | stmt                    { [$1] }
  | stmt stmt_list          { $1 :: $2 }

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

operator_list:
  | expr OPERATOR expr      {
    Ast.Call (Loc.span (Ast.loc_of_ast $1) (Ast.loc_of_ast $3),
              snd $2, [$1; $3])
  }

expr_list_inner:
  | { [] }
  | expr                       { [$1] }
  | expr COMMA expr_list_inner { ($1 :: $3) }

