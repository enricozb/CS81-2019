type id = string
type ty =
  | TyVar of tyvar ref
  | TyCon of id * (ty list)
  | TyFun of (ty list) * ty

and tyvar =
  | Link of ty
  | Unbound of id

val string_of_type : ty -> string

val typecheck : ty Env.env -> Ast.ast -> ty Env.env * ty

