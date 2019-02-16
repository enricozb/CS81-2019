val error : Loc.loc -> string -> 'a
type expr =
    Literal of value
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
    Nil
  | Bool of bool
  | Num of int
  | Sym of string
  | Pair of value * value
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
val parse_expr : Sexpr.expr -> annotated_expr
val parse_list_literal : Sexpr.expr -> value
val parse_binding : Sexpr.expr -> string * annotated_expr
val parse_formal : Sexpr.expr -> string
val is_fun_ty : Sexpr.expr list -> bool
val parse_type : Sexpr.expr -> Type.ml_type
val parse_arg : Sexpr.expr -> string
val parse_args : Sexpr.expr -> string list
val parse_tyscheme : Sexpr.expr -> Type.type_scheme
val parse_def : Sexpr.expr -> Loc.loc * def
