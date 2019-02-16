type ml_type =
    TyVar of string
  | TyCon of string
  | TyApp of ml_type * ml_type list

type type_scheme = ForAll of string list * ml_type

val eq_ty : ml_type -> ml_type -> bool
val eq_tys : ml_type list -> ml_type list -> bool
val render_ty : ml_type -> string
val render_tyscheme : type_scheme -> string

type subst = ml_type Env.env

val subst_id : 'a Env.StringMap.t
val subst_var : ml_type Env.StringMap.t -> Env.StringMap.key -> ml_type
val subst_ty : ml_type Env.StringMap.t -> ml_type -> ml_type
val subst_dom : 'a Env.StringMap.t -> Env.StringSet.t
val subst_compose :
  ml_type Env.StringMap.t ->
  ml_type Env.StringMap.t -> ml_type Env.StringMap.t
val instantiate_ty : type_scheme -> ml_type list -> ml_type
val free_tyvars : ml_type -> Env.StringSet.t
val canonicalize_tyscheme : type_scheme -> type_scheme

val num_ty : ml_type
val bool_ty : ml_type
val sym_ty : ml_type
val unit_ty : ml_type
val list_ty : ml_type -> ml_type
val ref_ty : ml_type -> ml_type
val pair_ty : ml_type -> ml_type -> ml_type

val funtype_of : ml_type list -> ml_type -> ml_type
val canonicalize_together : ml_type -> ml_type -> ml_type * ml_type
type type_env = type_scheme Env.env * Env.StringSet.t
val empty_tyenv : 'a Env.StringMap.t * Env.StringSet.t
val tyenv_find :
  Loc.loc -> 'a Env.StringMap.t * 'b -> Env.StringMap.key -> 'a
val tyenv_bind :
  type_scheme Env.StringMap.t * Env.StringSet.t ->
  Env.StringMap.key ->
  type_scheme -> type_scheme Env.StringMap.t * Env.StringSet.t

val fresh_counter : int ref
val fresh_tyvar : 'a -> ml_type
val fresh_conc_ty : 'a -> ml_type
val free_tyvars_tyenv : 'a * 'b -> 'b
val generalize_ty : ml_type -> Env.StringSet.t -> type_scheme
val generalize_free : ml_type -> type_scheme
val fresh_tyenv : type_scheme -> ml_type

type con = ConEq of ml_type * ml_type | ConAnd of con * con | ConTrivial

val render_con : con -> string
val free_tyvars_con : con -> Env.StringSet.t
val subst_con : ml_type Env.StringMap.t -> con -> con
val con_join : con list -> con
val con_solve : Loc.loc -> con -> ml_type Env.StringMap.t
val ty_as_general_as : Loc.loc -> type_scheme -> type_scheme -> bool

