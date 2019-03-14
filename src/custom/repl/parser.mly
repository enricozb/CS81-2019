%{
  let loc_of_token = function
    | NAME (l, _)
    | NUMBER (l, _)
    | IF l
    | ELSE l
    | WHILE l
    | DEF l
    | RETURN l
    | CHECKEXPECT l
    | CHECKERROR l
    | CHECKTYPEERROR l
    | IMPORT l
    | COLON l
    | DOT l
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

    let assoc_f i (_, op) = match op with
      | "or" -> (0, -i)
      | "and" -> (1, -i)
      | "<" | "<=" | ">" | ">=" | "!=" | "==" -> (2, -i)
      | "+" | "-" -> (3, -i)
      | "*" | "/" -> (4, -i)
      | "^" -> (5, i)
      | _ -> failwith "Unkown operator."

    let rec resolve_op_list left_ast op_list =
      let ops, asts = List.split op_list in
      let asts = left_ast :: asts in
      let assocs = List.mapi assoc_f ops in

      let ops = Array.of_list ops in
      let asts = Array.of_list asts in
      let assocs = Array.of_list assocs in

      let left_right idx arr assoc_or_ops =
        let offset = if assoc_or_ops then 0 else 1 in
        let left = Array.sub arr 0 (idx + offset) in
        let right = Array.sub arr (idx + 1) (Array.length arr - (idx + 1)) in
        (left, right)
      in

      let find arr el =
        let rec recurse n =
          if arr.(n) = el then
            n
          else
            recurse (n + 1)
        in
        recurse 0
      in

      let rec to_ast ops asts assocs =
        match Array.length ops with
          | 0 -> asts.(0)
          | 1 ->
            let (l, r) = (asts.(0), asts.(1)) in
            let loc = Loc.span (Ast.loc_of_ast l) (Ast.loc_of_ast r) in
            let op_loc, op = ops.(0) in
            Ast.Call (loc, Ast.Name (op_loc, op), Array.to_list asts)
          | _ ->
            (* get min associativity index, get the op associated with it,
             * split the asts and operators arrays
             * get locs for ast arrays
             * recurse *)
            let min_assoc = Array.fold_left min assocs.(0) assocs in
            (* min_assoc is a 2-tuple with the second element being either
             * plus or minus `idx`. So we can use `abs` to recover idx
             *)
            let idx = find assocs min_assoc in
            let op = ops.(idx) in
            let (l_asts, r_asts) = left_right idx asts false in
            let (l_ops, r_ops) = left_right idx ops true in
            let (l_assocs, r_assocs) = left_right idx assocs true in
            let loc = Loc.span
              (Ast.loc_of_ast @@ asts.(0))
              (Ast.loc_of_ast @@ asts.(Array.length asts - 1))
            in
            let op_loc, op = op in
            Ast.Call (loc, Ast.Name (op_loc, op),
              [to_ast l_ops l_asts l_assocs;
               to_ast r_ops r_asts r_assocs])
      in
      to_ast ops asts assocs

%}

%token <Loc.loc * string> NAME
%token <Loc.loc * string> NUMBER
%token <Loc.loc> CHECKEXPECT CHECKERROR CHECKTYPEERROR
%token <Loc.loc> IMPORT
%token <Loc.loc> IF ELSE
%token <Loc.loc> WHILE
%token <Loc.loc> DEF
%token <Loc.loc> RETURN
%token <Loc.loc> COLON DOT
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
  | flow_stmt NEWLINE       { $1 }

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
  | IMPORT import_name_list {
    Ast.Import (Loc.span $1 (fst $2), snd $2)
  }

import_name_list:
  | NAME                      { $1 }
  | NAME DOT import_name_list {
    (Loc.span (fst $1) (fst $3), (snd $1) ^ "/" ^ (snd $3))
  }

flow_stmt:
  | RETURN expr {
    Ast.Return (Loc.span $1 (Ast.loc_of_ast $2), $2)
  }

compound_stmt:
  | if_stmt                 { $1 }
  | while_stmt              { $1 }
  | funcdef                 { $1 }

(* TODO disallow stuff like `if True: x else: y` in one line *)
if_stmt:
  | IF expr COLON suite {
    Ast.If (Loc.span $1 (Ast.loc_of_ast_list $4),
            $2, $4, [])
  }
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
  | non_op_expr op_list         { resolve_op_list $1 (snd $2) }

op_list:
  | OPERATOR non_op_expr {
    (Loc.span (fst $1) (Ast.loc_of_ast $2)), [($1, $2)]
  }
  | OPERATOR non_op_expr op_list {
    (Loc.span (fst $1) (fst $3)), ($1, $2) :: (snd $3)
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
  | literal                 { $1 }

literal:
  | NUMBER                        { Ast.Num (fst $1, snd $1) }
  | LBRACK expr_list_inner RBRACK { Ast.List (Loc.span $1 $3, $2) }

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

