type id = string
type level = int

type ty =
  | TyVar of tyvar ref
  | TyCon of id * (ty list)
  | TyFun of (ty list) * ty
  | TyRecord of tyrow
	| TyRowEmpty
	| TyRowExtend of ty Ast.NameMap.t * tyrow

and tyvar =
  | Link of ty
  | Unbound of id * level
  | Generic of id

and tyrow = ty  (* kind of rows should only be TyRowEmpty or TyRowExtend *)


val string_of_type : ty -> string

val gen_var_ty : ty
val none_ty : ty
val bool_ty : ty
val int_ty : ty
val list_gen_ty : ty
val list_ty : ty -> ty
val fun_ty : ty list -> ty -> ty

val typecheck : ?level:level ->
  ty Env.env -> bool Env.env ->  (* the type env, and the mut env *)
  Ast.ast ->                     (* the ast to typecheck *)
  ty Env.env * bool Env.env * ty (* returns type env, mut env, the ast type *)

