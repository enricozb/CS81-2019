type id = string
type level = int

type ty =
  | TyVar of tyvar ref
  | TyCon of id * (ty list)
  | TyFun of (ty list) * ty
  | TyRecord of ty Ast.NameMap.t

  (* TyFold is for recursive types. The (id * (ty list)) portion is meant to *)
  (* be identical to TyCon. TyFold, if the first part of the tuple is not
   * None, is a TyCon with a TyRecord backing it. If the ty-con portion of
   * TyFold is None, then it's jus*)
  | TyFold of ((id * (ty list)) option) * (ty Lazy.t)
  | TyUnfold of ty

and tyvar =
  | Link of ty
  | Unbound of id * level * (traits option)
  | Generic of id * (traits option)

and traits = (ty BatDynArray.t)

type kind =
  | KindFun of kind_fun
  | KindVar of ty

and kind_fun = (ty list) -> ty


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

val gen_var_ty : ty
val gen_var_ty2 : ty
val none_ty : ty
val bool_ty : ty
val prim_fun_ty : ty list -> ty -> ty
val fun_ty : ty list -> ty -> ty
val callable_ty : ?level:level -> ty list -> ty -> ty
val has_field_ty : ?level:level ->
                   ?tycon:((id * (ty list)) option) -> string -> ty -> ty
val bare_record_ty : (string * ty) list -> ty
val folded_record_ty : ((id * (ty list)) option) -> (string * ty) list -> ty


val typecheck : ?level:level ->
  envs -> Ast.ast ->             (* the ast to typecheck *)
  envs * ty (* returns type env, mut env, the ast type *)

