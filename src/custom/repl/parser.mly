%{
  let loc_of_token = function
    | NAME (l, _)
    | NUMBER (l, _)
    | LET l | MUT l
    | IF l
    | ELSE l
    | WHILE l
    | BREAK l
    | CONTINUE l
    | DEF l
    | RETURN l
    | CLASS l
    | CHECKEXPECT l
    | CHECKERROR l
    | CHECKTYPE l
    | CHECKTYPEERROR l
    | IMPORT l
    | COLON l
    | DOT l
    | EQUALS l
    | MAPSTO l
    | COMMA l
    | STRING (l, _)
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

  let starts_with_upper s =
    let c = s.[0] in
    c = Char.uppercase_ascii c

  let starts_with_lower s =
    let c = s.[0] in
    c = Char.lowercase_ascii c
%}

%token <Loc.loc * string> NAME
%token <Loc.loc * string> NUMBER
%token <Loc.loc * string> STRING
%token <Loc.loc> CHECKEXPECT CHECKERROR CHECKTYPE CHECKTYPEERROR
%token <Loc.loc> IMPORT
%token <Loc.loc> LET MUT
%token <Loc.loc> IF ELSE
%token <Loc.loc> WHILE CONTINUE BREAK
%token <Loc.loc> DEF
%token <Loc.loc> RETURN
%token <Loc.loc> CLASS
%token <Loc.loc> COLON DOT
%token <Loc.loc> EQUALS MAPSTO
%token <Loc.loc> COMMA
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
  | check_stmt NEWLINE       { $1 }
  | bind_stmt NEWLINE        { $1 }
  | import_stmt NEWLINE      { $1 }
  | flow_stmt NEWLINE        { $1 }
  | expr NEWLINE             { $1 }
  | expr EQUALS expr NEWLINE {
    match $1 with
    | Ast.Name (l, name) ->
        Ast.Assign (l, name, $3)
    | Ast.Field (l, ast, name) ->
        Ast.SetField (l, ast, name, $3)
    | Ast.Call (l1, Ast.Field (l2, ast, "__getitem__"), [idx]) ->
        Ast.Call (l1, Ast.Field (l2, ast, "__setitem__"), [idx; $3])
    | _ ->
        failwith "Incorrect assignment"
  }
  | expr ASSIGNOPERATOR expr NEWLINE {
    let op_loc, op = $2 in
    let expr = Ast.Call (op_loc, Ast.Name (op_loc, op), [$1; $3]) in

    match $1 with
    | Ast.Name (l, name) ->
        Ast.Assign (l, name, expr)
    | Ast.Field (l, ast, name) ->
        Ast.SetField (l, ast, name, expr)
    | Ast.Call (l1, Ast.Field (l2, ast, "__getitem__"), [idx]) ->
        Ast.Call (l1, Ast.Field (l2, ast, "__setitem__"), [idx; expr])
    | _ ->
        failwith "Incorrect assignment"
  }

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
  | CHECKERROR expr {
    Ast.CheckError (Loc.span $1 (Ast.loc_of_ast $2), $2)
  }
  | CHECKTYPE expr {
    Ast.CheckType (Loc.span $1 (Ast.loc_of_ast $2), $2)
  }
  | CHECKTYPEERROR expr {
    Ast.CheckTypeError (Loc.span $1 (Ast.loc_of_ast $2), $2)
  }

bind_stmt:
  | LET MUT NAME EQUALS expr {
    if not (starts_with_lower (snd $3)) then
      failwith "Variables must start with a lower-case letter"
    else
      Ast.Bind (Loc.span $1 (Ast.loc_of_ast $5), true, snd $3, $5)
  }

  | LET NAME EQUALS expr {
    if not (starts_with_lower (snd $2)) then
      failwith "Variables must start with a lower-case letter"
    else
      Ast.Bind (Loc.span $1 (Ast.loc_of_ast $4), false, snd $2, $4)
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
  | RETURN {
    Ast.Return ($1, Ast.Name ($1, "none"))
  }
  | BREAK { Ast.Break $1 }
  | CONTINUE { Ast.Continue $1 }

compound_stmt:
  | if_stmt    { $1 }
  | while_stmt { $1 }
  | funcdef    { $1 }
  | classdef   { $1 }

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
  | DEF NAME trait_list name_list ret_ty COLON suite {
    if not (starts_with_lower (snd $2)) then
      failwith "Functions must start with a lower-case letter"
    else
      Ast.Def (Loc.span $1 (Ast.loc_of_ast_list $7),
               snd $2, $3, snd $4, $5, $7)
  }

  | DEF LPAREN OPERATOR RPAREN trait_list name_list ret_ty COLON suite {
      Ast.Def (Loc.span $1 (Ast.loc_of_ast_list $9),
               snd $3, $5, snd $6, $7, $9)
  }

ret_ty:
  | { None }
  | MAPSTO ty { Some $2 }

classdef:
  | CLASS NAME COLON suite {
    if not (starts_with_upper (snd $2)) then
      failwith "Class names must be capitalized"
    else
      let l = Loc.span $1 (Ast.loc_of_ast_list $4) in
      Ast.Class (l, snd $2, $4)
  }

trait_list:
  | { [] }
  | LANGLE RANGLE { [] }
  | LANGLE trait_list_inner RANGLE { $2 }

trait_list_inner:
  | NAME COLON ty                       {
    if not (starts_with_lower (snd $1)) then
      failwith "Type variables must be lowercase"
    else
      [(Ast.TyVar (fst $1, snd $1), $3)]
  }
  | NAME COLON ty COMMA trait_list_inner {
    if not (starts_with_lower (snd $1)) then
      failwith "Type variables must be lowercase"
    else
      (Ast.TyVar (fst $1, snd $1), $3) :: $5

  }

name_list:
  | LPAREN RPAREN                        { (Loc.span $1 $2, []) }
  | LPAREN name_list_inner RPAREN        { (Loc.span $1 $3, $2) }

name_list_inner:
  | NAME                                { [(snd $1, None)] }
  | NAME COLON ty                       { [(snd $1, Some $3)] }
  | NAME COMMA name_list_inner          { (snd $1, None) :: $3 }
  | NAME COLON ty COMMA name_list_inner { (snd $1, Some $3) :: $5 }

ty:
  | NAME {
    let name = (snd $1) in
    if starts_with_upper name then
      Ast.TyCon (fst $1, name, [])
    else
      Ast.TyVar (fst $1, name)
  }

  | NAME ty_list {
    let name = (snd $1) in
    if starts_with_lower name then
      failwith "Type variables must have kind *"
    else
      let loc = Loc.span (fst $1) (fst $2) in
      Ast.TyCon (loc, name, snd $2)
  }

ty_list:
  | LBRACK RBRACK               { (Loc.span $1 $2, []) }
  | LBRACK ty_list_inner RBRACK { (Loc.span $1 $3, $2) }

ty_list_inner:
  | ty                     { [$1] }
  | ty COMMA ty_list_inner { $1 :: $3 }

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
    | `Field (l, name) :: rest ->
      Ast.Field (l, iter rest, name)
    | `GetItem (l, idx) :: rest ->
      Ast.Call (l, Ast.Field (l, iter rest, "__getitem__"), [idx])
  in
  iter (List.rev $2)
  }

atom:
  | LPAREN expr RPAREN      { $2 }
  | NAME                    { Ast.Name (fst $1, snd $1) }
  | literal                 { $1 }

literal:
  | NUMBER {
    let loc = fst $1 in
    Ast.Call (loc, Ast.Name (loc, "Int"), [Ast.Num (fst $1, snd $1)])
  }
  | STRING {
    let loc = fst $1 in
    Ast.Call (loc, Ast.Name (loc, "String"), [Ast.String (fst $1, snd $1)])
  }
  | LBRACK expr_list_inner RBRACK {
    let loc = Loc.span $1 $3 in
    Ast.Call (loc, Ast.Name (loc, "List"), [Ast.List (loc, $2)])
  }
  | LBRACE named_expr_list_inner RBRACE {
    let name_ast_map =
      List.fold_right
        (fun (name, ast) map -> Ast.NameMap.add name ast map)
        $2
        Ast.NameMap.empty
    in
    Ast.Record (Loc.span $1 $3, name_ast_map)
  }

lambda:
  | name_list MAPSTO expr {
    Ast.Lambda (Loc.span (fst $1) (Ast.loc_of_ast $3), snd $1, $3)
  }

trailer_list:
  | { [] }
  | DOT NAME trailer_list {
    `Field (Loc.span $1 (fst $2), snd $2) :: $3
  }
  | LPAREN expr_list_inner RPAREN trailer_list {
    `Call (Loc.span $1 $3, $2) :: $4
  }
  | LBRACK expr RBRACK trailer_list {
    `GetItem (Loc.span $1 $3, $2) :: $4
  }

expr_list_inner:
  | { [] }
  | expr                       { [$1] }
  | expr COMMA expr_list_inner { ($1 :: $3) }

named_expr_list_inner:
  | { [] }
  | NAME COLON expr                             { [(snd $1, $3)] }
  | NAME COLON expr COMMA named_expr_list_inner { ((snd $1, $3) :: $5) }

