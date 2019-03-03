type id = string
type level = int
type ty =
  | TyVar of tyvar ref
  | TyCon of id * (ty list)
  | TyFun of (ty list) * ty

and tyvar =
  | Link of ty
  | Unbound of id * level
  | Generic of id

val string_of_type : ty -> string

val gen_var_ty : ty
val none_ty : ty
val bool_ty : ty
val int_ty : ty
val list_gen_ty : ty
val list_ty : ty -> ty
val fun_ty : ty list -> ty -> ty

val typecheck : ?level:level -> ty Env.env -> Ast.ast -> ty Env.env * ty
