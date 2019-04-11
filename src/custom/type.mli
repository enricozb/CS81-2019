type id = string
type level = int

type ty =
  | TyVar of tyvar ref
  | TyCon of id * (ty list)
  | TyFun of (ty list) * ty
  | TyRecord of tyrow
	| TyRowEmpty
	| TyRowExtend of ty Ast.NameMap.t * tyrow
  | TyFold of ((id * (ty list)) option) * (ty Lazy.t) (* for recursive types, namely classes *)
  | TyUnfold of ty

and tyvar =
  | Link of ty
  | Unbound of id * level
  | Generic of id

and tyrow = ty  (* kind of rows should only be TyRowEmpty or TyRowExtend *)


val string_of_type : ty -> string

val prim_int_ty : ty
val prim_string_ty : ty
val prim_list_gen_ty : ty
val prim_list_ty : ty -> ty

val gen_var_ty : ty
val gen_var_ty2 : ty
val none_ty : ty
val bool_ty : ty
val prim_fun_ty : ty list -> ty -> ty
val fun_ty : ty list -> ty -> ty
val callable_ty : ?level:level -> ?generic:bool -> ty list -> ty -> ty
val has_field_ty : ?level:level -> ?generic:bool -> string -> ty -> ty
val bare_record_ty : (string * ty) list -> ty
val folded_record_ty : ((id * (ty list)) option) -> (string * ty) list -> ty


val typecheck : ?level:level ->
  ty Env.env -> bool Env.env ->  (* the type env, and the mut env *)
  Ast.ast ->                     (* the ast to typecheck *)
  ty Env.env * bool Env.env * ty (* returns type env, mut env, the ast type *)

