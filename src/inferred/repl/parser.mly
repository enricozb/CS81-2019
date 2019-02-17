%{
  let loc_of_token = function
    | NAME (l, _)
    | NUMBER (l, _)
    | IF l
    | ELSE l
    | WHILE l
    | DEF l
    | CHECKEXPECT l
    | CHECKERROR l
    | CHECKTYPEERROR l
    | IMPORT l
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
    | ASSIGNOPERATOR (l, _)
    | INDENT l | DEDENT l | NEWLINE l
    | DEINDENT (l, _)
      -> l
    | EOF -> failwith "EOF has no `loc`"
%}

%token <Loc.loc * string> NAME
%token <Loc.loc * int> NUMBER
%token <Loc.loc> CHECKEXPECT CHECKERROR CHECKTYPEERROR
%token <Loc.loc> IMPORT
%token <Loc.loc> IF ELSE
%token <Loc.loc> WHILE
%token <Loc.loc> DEF
%token <Loc.loc> COLON
%token <Loc.loc> EQUALS MAPSTO
%token <Loc.loc> COMMA QUOTE
%token <Loc.loc> LANGLE RANGLE
%token <Loc.loc> LBRACE RBRACE
%token <Loc.loc> LPAREN RPAREN
%token <Loc.loc> LBRACK RBRACK
%token <Loc.loc * string> OPERATOR
%token <Loc.loc * string> ASSIGNOPERATOR
%token <Loc.loc> INDENT DEDENT NEWLINE
%token <Loc.loc * int> DEINDENT
%token EOF

%start file_input
%start single_input

%type <Ast.ast> single_input
%type <Ast.ast list> file_input

%%

file_input:
  | NEWLINE file_input      { $2 }
  | stmt file_input         { $1 :: $2 }
  | EOF                     { [] }

single_input:
  | NEWLINE single_input    { $2 }
  | simple_stmt             { $1 }
(* for interactive input NEWLINE must be after compound_stmt *)
  | compound_stmt NEWLINE   { $1 }

stmt:
  | simple_stmt             { $1 }
  | compound_stmt           { $1 }

simple_stmt:
  | expr NEWLINE            { $1 }
  | check_stmt NEWLINE      { $1 }
  | assign_stmt NEWLINE     { $1 }
  | import_stmt NEWLINE     { $1 }

suite:
  | simple_stmt                     { [$1] }
  | NEWLINE INDENT stmt_list DEDENT { $3 }

stmt_list:
  | stmt                    { [$1] }
  | stmt stmt_list          { $1 :: $2 }

check_stmt:
  | CHECKEXPECT expr COMMA expr   {
    Ast.CheckExpect (Loc.span $1 (Ast.loc_of_ast $4), $2, $4)
  }
  (* TODO types don't carry locs, so I can't span here... *)
  | CHECKERROR expr {
    Ast.CheckError (Loc.span $1 (Ast.loc_of_ast $2), $2)
  }
  | CHECKTYPEERROR expr {
    Ast.CheckTypeError (Loc.span $1 (Ast.loc_of_ast $2), $2)
  }

assign_stmt:
  | NAME EQUALS expr {
    Ast.Bind (Loc.span (fst $1) (Ast.loc_of_ast $3), snd $1, $3)
  }

  | NAME ASSIGNOPERATOR expr {
    let loc_name, name = $1 in
    let loc_op, op = $2 in
    Ast.Bind (Loc.span loc_name (Ast.loc_of_ast $3),
              name,
              Ast.Call (loc_op, Ast.Name (loc_op, op),
                        [Ast.Name (loc_name, name); $3]))
  }

import_stmt:
  | IMPORT NAME {
    Ast.Import (Loc.span $1 (fst $2), snd $2)
  }

compound_stmt:
  | if_stmt                 { $1 }
  | while_stmt              { $1 }
  | funcdef                 { $1 }

(* TODO disallow stuff like `if True: x else: y` in one line *)
if_stmt:
  | IF expr COLON suite ELSE COLON suite {
    Ast.If (Loc.span $1 (Ast.loc_of_ast_list $7),
            $2, $4, $7)
  }

while_stmt:
  | WHILE expr COLON suite {
    Ast.While (Loc.span $1 (Ast.loc_of_ast_list $4),
               $2, $4)
  }

funcdef:
  | DEF NAME name_list COLON suite {
    Ast.Def (Loc.span $1 (Ast.loc_of_ast_list $5),
             snd $2, snd $3, $5)
  }

name_list:
  | LPAREN RPAREN                        { (Loc.span $1 $2, []) }
  | LPAREN name_list_inner RPAREN        { (Loc.span $1 $3, $2) }

name_list_inner:
  | NAME                 { [snd $1] }
  | NAME COMMA name_list_inner { (snd $1) :: $3 }

expr:
  | non_op_expr                 { $1 }
  | non_op_expr OPERATOR expr   {
    Ast.Call (Loc.span (Ast.loc_of_ast $1) (Ast.loc_of_ast $3),
              Ast.Name (fst $2, snd $2), [$1; $3])
  }

non_op_expr:
  | lambda                  { $1 }
  | atom_expr               { $1 }

atom_expr:
  | atom trailer_list       {
  let rec iter = function
    | [] -> $1
    | `Call (l, params) :: rest ->
      Ast.Call (l, iter rest, params)
  in
  iter (List.rev $2)
  }

atom:
  | LPAREN expr RPAREN      { $2 }
  | NAME                    { Ast.Name (fst $1, snd $1) }
  (* TODO change to literal when adding lists *)
  | NUMBER       { Ast.Num (fst $1, snd $1) }

lambda:
  | name_list MAPSTO expr {
    Ast.Lambda (Loc.span (fst $1) (Ast.loc_of_ast $3), snd $1, $3)
  }

trailer_list:
  | { [] }
  | LPAREN expr_list_inner RPAREN trailer_list {
    `Call (Loc.span $1 $3, $2) :: $4
  }

expr_list_inner:
  | { [] }
  | expr                       { [$1] }
  | expr COMMA expr_list_inner { ($1 :: $3) }

