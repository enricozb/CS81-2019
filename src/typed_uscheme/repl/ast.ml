open Loc

type ty =
  | TyStr of string
  | TyApp of string * ty list
  | TyFun of (ty list) * ty

type name = string
type tyvar = string
type typed_namelist = (name * ty) list

type ast =
  | Name of loc * name
  | Num of loc * int
  | Lambda of (loc * typed_namelist * ast)
  | Call of (loc * name * (ast list))
  | Bind of (loc * name * ast)
  | Def of (loc * name * (tyvar list) * (typed_namelist) * ty * (ast list))

let rec string_of_type = function
  | TyStr s -> s
  | TyFun (types, rtype) ->
      let inner_types =
        let rec iter = function
          | [] -> ""
          | ty :: [] -> (string_of_type ty)
          | ty :: rest -> (string_of_type ty) ^ " " ^ (iter rest)
        in iter types
      in
      "(" ^ inner_types ^ " -> " ^ (string_of_type rtype) ^ ")"
  | TyApp (s, types) ->
      let inner_types =
        let rec iter = function
          | [] -> ""
          | ty :: [] -> (string_of_type ty)
          | ty :: rest -> (string_of_type ty) ^ ", " ^ (iter rest)
        in iter types
      in
      "<" ^ inner_types ^ ">"

let rec string_of_str_list lst sep =
  let rec iter = function
    | [] -> ""
    | (el :: []) -> el
    | (el :: x) -> el ^ sep ^ (iter x)
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
  | Call (_, name, params) -> name ^ "(" ^ (string_of_ast_list params ",") ^ ")"
  | Lambda (_, typed_params, stmt) ->
      "(" ^ string_of_typed_namelist typed_params ", " ^ ") -> " ^ string_of_ast stmt
  | Def (_, funcname, type_vars, typed_params, rtype, stmts) ->
      "def " ^ funcname ^
        "(" ^ string_of_typed_namelist typed_params ", " ^ "): ..."
  | Bind (_, name, expr) -> name ^ " = " ^ string_of_ast expr

let print_ast ast = Printf.printf "%s\n" (string_of_ast ast)

let loc_of_ast = function
  | Name (l, _)
  | Num (l, _)
  | Call (l, _, _)
  | Lambda (l, _, _)
  | Def (l, _, _, _, _, _)
  | Bind (l, _, _)
    -> l

let loc_of_ast_list = function
  | [] -> failwith "loc_of_ast_list with empty list"
  | ast :: rest ->
      let rec loc_of_ast_ loc = function
        | [] -> loc
        | ast :: rest -> loc_of_ast_ (Loc.span loc @@ loc_of_ast ast) rest
      in loc_of_ast_ (loc_of_ast ast) rest

