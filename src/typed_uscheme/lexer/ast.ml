type ty = string
type name = string
type typed_namelist = (name * ty) list

type ast =
  | Identifier of name
  | Num of int
  | Lambda of (typed_namelist * ast)
  | Call of (name * (ast list))
  | Assign of (name * ast)
  (* | If of (ast * (ast list) * (ast list)) *)
  | Def of (name * (typed_namelist) * name * (ast list))

let rec string_of_str_list lst sep =
  let rec iter = function
    | [] -> ""
    | (el :: []) -> el
    | (el :: x) -> el ^ sep ^ (iter x)
  in iter lst

let rec string_of_typed_namelist lst sep =
  let rec iter = function
    | [] -> ""
    | ((name, typ) :: []) -> name ^ ": " ^ typ
    | ((name, typ) :: x) -> name ^ ": " ^ typ ^ sep ^ (iter x)
  in iter lst

let rec string_of_ast_list lst sep =
  let rec iter = function
    | [] -> ""
    | (el :: []) -> string_of_ast el
    | (el :: x) -> (string_of_ast el) ^ sep ^ (iter x)
  in iter lst

and string_of_ast = function
  | Name s -> s
  | None -> "None"
  | Bool b -> string_of_bool b
  | Num i -> string_of_int i
  | String s -> "\"" ^ s ^ "\""
  | List l -> "[" ^ (string_of_ast_list l ",")  ^ "]"
  | Call (name, params) -> name ^ "(" ^ (string_of_ast_list params ",") ^ ")"
  | If (test, stmts) ->
      "if " ^ (string_of_ast test) ^ ":\n~INDENT~\n" ^
        (string_of_ast_list stmts "\n") ^ "\n~DEDENT~"
  | Lambda (params, stmt) ->
      "(" ^ string_of_str_list params "," ^ ") -> " ^ string_of_ast stmt
  | Def (funcname, typed_params, rtype, stmts) ->
      "def " ^ funcname ^
        "(" ^ string_of_typed_namelist typed_params "," ^ "): ..."
  | Bind (name, expr) -> name ^ " = " ^ string_of_ast expr

let print_ast ast = Printf.printf "%s\n" (string_of_ast ast)

let is_expression = function
  | Bind _
  | Def _ -> false
  | _ -> true

let rec ast_to_ast2 = function
  | Identifier of name
  | Num of int
  | Lambda of (typed_namelist * ast)
  | Call of (name * (ast list))
  | Assign of (name * ast)
  (* | If of (ast * (ast list) * (ast list)) *)
  | Def of (name * (typed_namelist) * name * (ast list))

