open Env
open Type
open Error

let error = Error.syntax_err

type expr = Literal of value
          (* Not a literal... should be *)
          | List of annotated_expr list
          | Var of string
          | If of annotated_expr * annotated_expr * annotated_expr
          | Begin of annotated_expr list
          | Call of annotated_expr * annotated_expr list
          | LetX of let_kind * (string * annotated_expr) list * annotated_expr
          | Lambda of lambda

and let_kind = Let | LetRec | LetStar

and lambda = string list * annotated_expr

and annotated_expr = Loc.loc * expr

and value = Nil
          | Bool of bool
          | Num of int
          | Sym of string
          | Pair of value * value
          | ListVal of value list
          | Closure of lambda * (unit -> value env)
          | Primitive of primop
          | Ref of value ref

and primop = value list -> Loc.loc -> value

let eq_val a b = match (a, b) with
    | (Nil, Nil) -> true
    | (Num a, Num b) -> a = b
    | (Bool a, Bool b) -> a = b
    | (Sym a, Sym b) -> a = b
    (* TODO: Handle pair, closure, etc. in error? *)
    | _ -> false

let rec render_val = function
  | Nil -> "'()"
  | Bool b -> if b then "true" else "false"
  | Num i -> string_of_int i
  | Sym s -> s
   (* How should we render pairs versus lists? *)
  | Pair (a, b) ->
    "'(" ^ render_pair a b ^ ")"
  | ListVal vals ->
    "[" ^ render_list_vals vals ^ "]"
  | Closure _ -> "<closure>"
  | Primitive _ -> "<primitive>"
  | Ref r -> "ref " ^ (render_val !r)

and render_pair a =
  let a = render_val a in function
    | Nil -> a
    | Pair (b, c) -> a ^ " " ^ render_pair b c
    | other -> a ^ " . " ^ render_val other

and render_list_vals = function
  | [] -> ""
  | [x] -> render_val x
  | x :: xs -> render_val x ^ ", " ^ render_list_vals xs

type def = Val of string * annotated_expr
         | ValRec of string * annotated_expr
         | Expr of annotated_expr
         | Define of string * string list * annotated_expr
         | Use of string
         | CheckExpect of annotated_expr * annotated_expr
         | CheckError of annotated_expr
         | CheckType of annotated_expr * type_scheme
         | CheckPrincipalType of annotated_expr * type_scheme
         | CheckTypeError of annotated_expr

and annotated_def = Loc.loc * def

let rec parse_expr = function
  | Ast.Name (l, name) -> (l, Var name)
  | Ast.Num (l, i) -> (l, Literal (Num i))
  | Ast.List (l, vals) -> (l, List (parse_expr_list vals))
  | Ast.Lambda (l, params, expr) -> (l, Lambda (params, parse_expr expr))
  | Ast.Call (l, func, params) ->
      (l, Call (parse_expr func, parse_expr_list params))
  | Ast.Bind _ -> failwith "parse_expr: Ast.Bind, should be within a block"
  | Ast.If (l, test, t_suite, f_suite) ->
      (l, If (parse_expr test, parse_suite t_suite, parse_suite f_suite))
  | Ast.While (l, test, suite) -> failwith "parse_expr: while not implemented"
  | _ -> failwith "not implemented"

and parse_expr_list = function
  | [] -> []
  | expr :: rest -> (parse_expr expr) :: (parse_expr_list rest)

(* to turn Ast.Bind into LetX *)
and parse_suite suite =
  if suite == [] then
    failwith "Empty body or useless let expression"
  else
    let suite_loc =
      Loc.span
        (Ast.loc_of_ast (List.nth suite 0))
        (Ast.loc_of_ast (List.nth suite (List.length suite - 1)))
    in
    (suite_loc, Begin (parse_inner_suite suite))

and parse_inner_suite = function
  | [] -> []
  | (Ast.Bind (l, name, expr)) :: rest ->
      [(l, LetX (Let, [(name, parse_expr expr)], (parse_suite rest)))]
  | expr :: rest -> (parse_expr expr) :: (parse_inner_suite rest)

let parse_def = function
  | Ast.Import (l, name) -> (l, Use name)
  | Ast.Def (l, name, params, body) ->
      let lambda = (l, Lambda (params, parse_suite body)) in
      (l, ValRec (name, lambda))
  | Ast.Bind (l, name, expr) -> (l, Val (name, parse_expr expr))
  | expr ->
      let (l, expr) = parse_expr expr in
      (l, Expr (l, expr))

