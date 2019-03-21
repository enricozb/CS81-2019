open Loc

type name = string

type ast =
  | Name of Loc.loc * name
  | Num of Loc.loc * string
  | List of Loc.loc * (ast list)
  | Lambda of (Loc.loc * (name list) * ast)
  | Call of (Loc.loc * ast * (ast list))
  | Bind of (Loc.loc * bool * name * ast)
  | Assign of (Loc.loc * name * ast)
  | If of (Loc.loc * ast * (ast list) * (ast list))
  | While of (Loc.loc * ast * (ast list))
  | Break of Loc.loc
  | Continue of Loc.loc
  | Def of (Loc.loc * name * (name list) * (ast list))
  | Return of (Loc.loc * ast)
  | Suite of (Loc.loc * (ast list)) (* used only oustide of parser *)
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

(* TODO : change this to `to_string` *)
and string_of_ast = function
  | Name (_, s) -> s

  | Num (_, i) -> i

  | List (_, exprs) ->
      "[" ^ string_of_ast_list exprs ", " ^ "]"

  | Call (_, expr, params) ->
      string_of_ast expr ^ "(" ^ (string_of_ast_list params ", ") ^ ")"

  | Lambda (_, params, stmt) ->
      "(" ^ string_of_str_list params ", " ^ ") -> " ^ string_of_ast stmt

  | If (_, expr, true_body, false_body) ->
      "if " ^ string_of_ast expr ^ ": ... "

  | While (_, expr, body) ->
      "while " ^ string_of_ast expr ^ ": ... "

  | Break l -> "break"

  | Continue l -> "continue"

  | Bind (_, mut, name, expr) when mut ->
      "let mut " ^ name ^ " = " ^ string_of_ast expr
  | Bind (_, _, name, expr) ->
      "let " ^ name ^ " = " ^ string_of_ast expr

  | Assign (_, name, expr) ->
      name ^ " = " ^ string_of_ast expr

  | Def (_, funcname, params, stmts) ->
      "def " ^ funcname ^ "(" ^ string_of_str_list params ", " ^ "): ..."

  | Return (_, ast) ->
      "return " ^ (string_of_ast ast)

  | Import (_, name) ->
      "import " ^ name

  | Suite (l, asts) -> "<suite>"

  | CheckExpect (l, ast1, ast2) ->
      "check_expect " ^ string_of_ast ast1 ^ ", " ^ string_of_ast ast2

  | CheckError (l, ast) ->
      "check_error " ^ string_of_ast ast

  | CheckTypeError (l, ast) ->
      "check_type_error " ^ string_of_ast ast

let print_ast ast = Printf.printf "%s\n" (string_of_ast ast)

let loc_of_ast = function
  | Name (l, _)
  | Num (l, _)
  | List (l, _)
  | Call (l, _, _)
  | Lambda (l, _, _)
  | If (l, _, _, _)
  | While (l, _, _)
  | Break l
  | Continue l
  | Assign (l, _, _)
  | Bind (l, _, _, _)
  | Def (l, _, _, _)
  | Return (l, _)
  | Suite (l, _)
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

let is_expr = function
  | Name _ | Num _ | List _ | Call _ | Lambda _ -> true
  | If _ 
  | While _ 
  | Break _ 
  | Continue _ 
  | Assign _ 
  | Bind _ 
  | Def _ 
  | Return _ 
  | Suite _ 
  | Import _ 
  | CheckExpect _ 
  | CheckError _ 
  | CheckTypeError _ 
    -> false

