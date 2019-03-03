type id = string

type ty =
  | TyVar of id
  | TyCon of id * (ty list)
  | TyFun of (ty list) * ty

type constraints = (ty * ty) list

val string_of_type : ty -> string

val typecheck : ty Env.env -> Ast.ast -> ty Env.env * ty

