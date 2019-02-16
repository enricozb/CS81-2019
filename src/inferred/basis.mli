val truthy : 'a -> Syntax.value -> bool
val unary_fun : (Syntax.value -> Loc.loc -> Syntax.value) -> Syntax.value
val binary_fun :
  (Syntax.value -> Syntax.value -> Loc.loc -> Syntax.value) -> Syntax.value
val binary_num_fun : (int -> int -> Syntax.value) -> Syntax.value
val num_fun_to_num : (int -> int -> int) -> Syntax.value
val num_fun_to_bool : (int -> int -> bool) -> Syntax.value
val cons : Syntax.value
val car : Syntax.value
val cdr : Syntax.value
val refer : Syntax.value
val deref : Syntax.value
val set : Syntax.value
val print : Syntax.value
val error : Syntax.value
val eq : Syntax.value
val fst : Syntax.value
val snd : Syntax.value
val is_null : Syntax.value
val prim_env : Syntax.value Env.StringMap.t
val alpha : Type.ml_type
val alpha_list : Type.ml_type
val alpha_ref : Type.ml_type
val beta : Type.ml_type
val unary_fun_sig : Type.ml_type -> Type.ml_type -> Type.ml_type
val binary_fun_sig : Type.ml_type -> Type.ml_type -> Type.ml_type
val prim_sigs : (string * Type.ml_type) list
val prim_gamma : Type.type_scheme Env.StringMap.t * Env.StringSet.t
val nanoml_basis : string
