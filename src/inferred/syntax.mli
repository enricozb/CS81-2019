val error : Loc.loc -> string -> 'a

type expr =
  | Literal of value
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
and value =
  | Nil
  | Bool of bool
  | Num of int
  | Sym of string
  | Pair of value * value
  | ListVal of value list
  | Closure of lambda * (unit -> value Env.env)
  | Primitive of primop
  | Ref of value ref
and primop = value list -> Loc.loc -> value

val eq_val : value -> value -> bool
val render_val : value -> string
val render_pair : value -> value -> string

type def =
    Val of string * annotated_expr
  | ValRec of string * annotated_expr
  | Expr of annotated_expr
  | Define of string * string list * annotated_expr
  | Use of string
  | CheckExpect of annotated_expr * annotated_expr
  | CheckError of annotated_expr
  | CheckType of annotated_expr * Type.type_scheme
  | CheckPrincipalType of annotated_expr * Type.type_scheme
  | CheckTypeError of annotated_expr
and annotated_def = Loc.loc * def

val parse_expr : Ast.ast -> annotated_expr
val parse_def : Ast.ast -> annotated_def
(*val parse_list_literal : Ast.ast -> value*)
(*val parse_binding : Ast.ast -> string * annotated_expr*)
(*val parse_formal : Ast.ast -> string*)
(*val is_fun_ty : Ast.ast list -> bool*)
(*val parse_type : Ast.ast -> Type.ml_type*)
(*val parse_arg : Ast.ast -> string*)
(*val parse_args : Ast.ast -> string list*)
(*val parse_tyscheme : Ast.ast -> Type.type_scheme*)

