open Loc

type name = string

type ast =
  | Name of Loc.loc * name
  | Num of Loc.loc * int
  | Lambda of (Loc.loc * (name list) * ast)
  | Call of (Loc.loc * ast * (ast list))
  | Bind of (Loc.loc * name * ast)
  | If of (Loc.loc * ast * (ast list) * (ast list))
  | While of (Loc.loc * ast * (ast list))
  | Def of (Loc.loc * name * (name list) * (ast list))
  | Import of (Loc.loc * name)
  | CheckExpect of (Loc.loc * ast * ast)
  | CheckError of (Loc.loc * ast)
  | CheckTypeError of (Loc.loc * ast)

let rec string_of_str_list lst sep =
  let rec iter = function
    | [] -> ""
    | (el :: []) -> el
    | (el :: x) -> el ^ sep ^ (iter x)
  in iter lst

let rec string_of_ast_list lst sep =
  let rec iter = function
    | [] -> ""
    | (el :: []) -> string_of_ast el
    | (el :: x) -> (string_of_ast el) ^ sep ^ (iter x)
  in iter lst

and string_of_ast = function
  | Name (_, s) -> s
  | Num (_, i) -> string_of_int i
  | Call (_, expr, params) ->
      string_of_ast expr ^ "(" ^ (string_of_ast_list params ",") ^ ")"
  | Lambda (_, params, stmt) ->
      "(" ^ string_of_str_list params ", " ^ ") -> " ^ string_of_ast stmt
  | If (_, expr, true_body, false_body) ->
      "if " ^ string_of_ast expr ^ ": ... "
  | While (_, expr, body) ->
      "while " ^ string_of_ast expr ^ ": ... "
  | Bind (_, name, expr) -> name ^ " = " ^ string_of_ast expr
  | Def (_, funcname, params, stmts) ->
      "def " ^ funcname ^ "(" ^ string_of_str_list params ", " ^ "): ..."
  | Import (_, name) ->
      "import " ^ name

let print_ast ast = Printf.printf "%s\n" (string_of_ast ast)

let loc_of_ast = function
  | Name (l, _)
  | Num (l, _)
  | Call (l, _, _)
  | Lambda (l, _, _)
  | If (l, _, _, _)
  | While (l, _, _)
  | Bind (l, _, _)
  | Def (l, _, _, _)
  | Import (l, _)
  | CheckExpect (l, _, _)
  | CheckError (l, _)
  | CheckTypeError (l, _)
    -> l

let loc_of_ast_list = function
  | [] -> failwith "loc_of_ast_list with empty list"
  | ast :: rest ->
      let rec loc_of_ast_ loc = function
        | [] -> loc
        | ast :: rest -> loc_of_ast_ (Loc.span loc @@ loc_of_ast ast) rest
      in loc_of_ast_ (loc_of_ast ast) rest

