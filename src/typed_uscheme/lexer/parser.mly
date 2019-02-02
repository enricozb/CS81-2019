 /* File parser.mly */
%token <string> TYPE
%token <string> IDENTIFIER
%token <int> NUMBER
%token IF
%token DEF
%token COLON
%token EQUALS MAPSTO
%token COMMA
%token LANGLE RANGLE
%token LBRACE RBRACE
%token LPAREN RPAREN
%token LBRACK RBRACK
%token <string> OPERATOR
%token INDENT DEDENT NEWLINE
%token <int> DEINDENT
%token EOF
%start main             /* the entry point */
%type <Ast.ast> main

%%
main:
  | NEWLINE main            { $2 }
  | stmt                    { $1 }

stmt:
  | simple_stmt             { $1 }
  | compound_stmt           { $1 }

simple_stmt:
  | expr NEWLINE            { $1 }
  | assign_stmt NEWLINE     { $1 }

assign_stmt:
  | NAME EQUALS expr        { Ast.Bind ($1, $3) }

compound_stmt:
  (* | if_stmt                 { $1 } *)
  | funcdef                 { $1 }

(*
if_stmt:
  | IF expr COLON suite     { Ast.If ($2, $4) }
  *)

funcdef:
  | DEF NAME generic_list typed_namelist MAPSTO TYPE COLON suite {
    Ast.Def ($2, $4, $6, $8)
  }

generic_list:
  | LANGLE TYPE RANGLE       { [$2] }

namelist:
  | NAME                     { [$1] }
  | NAME COMMA namelist      { $1 :: $3 }

typed_namelist:
  | LPAREN RPAREN                        { [] }
  | LPAREN typed_namelist_inner RPAREN   { $2 }

typed_namelist_inner:
  | NAME COLON TYPE                            { [($1, $3)] }
  | NAME COLON TYPE COMMA typed_namelist_inner { ($1, $3) :: $5 }

suite:
  | simple_stmt             { [$1] }
  | NEWLINE INDENT stmt_list DEDENT { $3 }

stmt_list:
  | stmt                    { [$1] }
  | stmt stmt_list          { $1 :: $2 }

expr:
  | literal                 { $1 }
  | NAME                    { Ast.Name $1 }
  | lambda                  { $1 }
  | call                    { $1 }
  | LPAREN expr RPAREN      { $2 }

literal:
  | NUMBER       { Ast.Num $1 }

lambda:
  | typed_namelist MAPSTO expr { Ast.Lambda ($1, $3) }

call:
  | NAME LPAREN expr_list_inner RPAREN { Ast.Call ($1, $3) }

expr_list_inner:
  | { [] }
  | expr                       { [$1] }
  | expr COMMA expr_list_inner { ($1 :: $3) }

