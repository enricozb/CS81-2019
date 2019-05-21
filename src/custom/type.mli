type id = string
type level = int

type ty =
  | TyVar of tyvar ref
  | TyCon of id * (ty list)
  | TyFun of (ty list) * ty
  | TyRecord of tyrow
  | TyRowEmpty
  | TyRowExtend of ty Ast.NameMap.t * tyrow
  | TyFold of ((id * (ty list)) option) * (ty Lazy.t) (* for recursive types *)
  | TyUnfold of ty

and tyvar =
  | Link of ty
  | Unbound of id * level * (trait option)
  | Generic of id * (trait option)

and kind =
  | KindFun of kind_fun
  | KindVar of ty
  | KindTrait of kind_trait

and kind_fun = (ty list) -> ty
and kind_trait = level -> ty -> (ty list) -> ty

(* a mutable TyFold *)
and trait = (string ref) * ((ty list) ref) * (ty Lazy.t)

and tyrow = ty (* kind of rows should only be TyRowEmpty or TyRowExtend *)

val kind_fun_0 : ty -> kind_fun
val kind_fun_1 : (ty -> ty) -> kind_fun
(*val kind_fun_2 : (ty -> ty -> ty) -> kind_fun*)

type envs = {
  ty_env : ty Env.env;
  kind_env : kind Env.env;
  mut_env : bool Env.env;
  val_env : Value.env_value Env.env;
}

val string_of_type : ty -> string

val prim_int_ty : ty
val prim_string_ty : ty
val prim_list_gen_ty : ty
val prim_list_ty : ty -> ty

val int_ty : ty ref
val string_ty : ty ref
val list_of_ty : (ty -> ty) ref
val list_ty : ty ref

(* convenience types *)
val gen_var_ty : ty
val gen_var_ty2 : ty
val none_ty : ty
val bool_ty : ty
val prim_fun_ty : ty list -> ty -> ty
val fun_ty : ty list -> ty -> ty
val bare_record_ty : (string * ty) list -> ty
val folded_record_ty : ((id * (ty list)) option) -> (string * ty) list -> ty

val extensible_field_ty : ?level:level -> ?generic:bool -> string -> ty -> ty

(* convenience traits *)
(* TODO: i don't think `generic` is needed here *)
val has_field_trait : ?level:level -> ?generic:bool -> string -> ty -> ty
val callable_trait : ?level:level -> ?generic:bool -> ty list -> ty -> ty

val typecheck : ?level:level ->
  envs -> Ast.ast ->             (* the ast to typecheck *)
  envs * ty (* returns type env, mut env, the ast type *)

