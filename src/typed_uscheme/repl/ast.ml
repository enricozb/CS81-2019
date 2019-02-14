open Loc

type ty =
  | TyVar    of string
  | TyStr    of string
  | TyApp    of string * ty list
  | TyFun    of (ty list) * ty
  | TyForAll of (string list) * ty

type name = string
type tyvar = string
type typed_namelist = (name * ty) list

type ast =
  | Name of loc * name
  | Num of loc * int
  | Lambda of (loc * typed_namelist * ast)
  | TypeLambda of (loc * (tyvar list) * ast)
  | Call of (loc * ast * (ast list))
  | Instantiation of (loc * ast * (ty list))
  | Bind of (loc * name * ast)
  | If of (loc * ast * (ast list) * (ast list))
  | While of (loc * ast * (ast list))
  | Def of (loc * name * (tyvar list) * (typed_namelist) * ty * (ast list))
  | Import of (loc * name)

let rec string_of_str_list lst sep =
  let rec iter = function
    | [] -> ""
    | (el :: []) -> el
    | (el :: x) -> el ^ sep ^ (iter x)
  in iter lst

let rec string_of_type = function
  | TyVar s -> "'" ^ s
  | TyStr s -> s
  (* TODO this is not the right stringification of function types... *)
  | TyFun (types, rtype) ->
      "(" ^ string_of_type_list types ", " ^ " -> " ^ (string_of_type rtype) ^ ")"
  | TyApp (s, types) ->
      s ^ "<" ^ string_of_type_list types ", " ^ ">"
  | TyForAll (typevars, ty) ->
      "forall '" ^ string_of_str_list typevars ", '" ^ " " ^ string_of_type ty

and string_of_type_list lst sep =
  let rec iter = function
    | [] -> ""
    | (el :: []) -> string_of_type el
    | (el :: x) -> (string_of_type el) ^ sep ^ (iter x)
  in iter lst

let rec string_of_typed_namelist lst sep =
  let rec iter = function
    | [] -> ""
    | ((name, typ) :: []) -> name ^ ": " ^ (string_of_type typ)
    | ((name, typ) :: x) -> name ^ ": " ^ (string_of_type typ) ^ sep ^ (iter x)
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
  | Instantiation (_, expr, types) ->
      string_of_ast expr ^ "<" ^ string_of_type_list types ", " ^ ">"
  | Lambda (_, typed_params, stmt) ->
      "(" ^ string_of_typed_namelist typed_params ", " ^ ") -> " ^ string_of_ast stmt
  | TypeLambda (_, type_vars, expr) ->
      "forall<'" ^ string_of_str_list type_vars ", '" ^ "> -> " ^ string_of_ast expr
  | If (_, expr, true_body, false_body) ->
      "if " ^ string_of_ast expr ^ ": ... "
  | While (_, expr, body) ->
      "while " ^ string_of_ast expr ^ ": ... "
  | Bind (_, name, expr) -> name ^ " = " ^ string_of_ast expr
  | Def (_, funcname, type_vars, typed_params, rtype, stmts) ->
      "def " ^ funcname ^
        "(" ^ string_of_typed_namelist typed_params ", " ^ "): ..."
  | Import (_, name) ->
      "import " ^ name

let print_ast ast = Printf.printf "%s\n" (string_of_ast ast)

let loc_of_ast = function
  | Name (l, _)
  | Num (l, _)
  | Call (l, _, _)
  | Instantiation (l, _, _)
  | Lambda (l, _, _)
  | TypeLambda (l, _, _)
  | If (l, _, _, _)
  | While (l, _, _)
  | Bind (l, _, _)
  | Def (l, _, _, _, _, _)
  | Import (l, _)
    -> l

let loc_of_ast_list = function
  | [] -> failwith "loc_of_ast_list with empty list"
  | ast :: rest ->
      let rec loc_of_ast_ loc = function
        | [] -> loc
        | ast :: rest -> loc_of_ast_ (Loc.span loc @@ loc_of_ast ast) rest
      in loc_of_ast_ (loc_of_ast ast) rest

