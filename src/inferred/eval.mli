val typecheck_expr :
  Type.type_scheme Env.StringMap.t * Env.StringSet.t ->
  Syntax.annotated_expr -> Type.ml_type * Type.con
val typecheck_exprs :
  Type.type_scheme Env.StringMap.t * Env.StringSet.t ->
  Syntax.annotated_expr list -> Type.ml_type list * Type.con
val typecheck_literal :
  Loc.loc ->
  Type.type_scheme Env.StringMap.t * Env.StringSet.t ->
  Syntax.value -> Type.ml_type * Type.con
val typecheck_def :
  Type.type_scheme Env.StringMap.t * Env.StringSet.t ->
  Loc.loc * Syntax.def ->
  (Type.type_scheme Env.StringMap.t * Env.StringSet.t) * Type.type_scheme
val eval_expr :
  Syntax.value Env.StringMap.t -> Syntax.annotated_expr -> Syntax.value
val eval_def :
  Syntax.value Env.StringMap.t ->
  Loc.loc * Syntax.def -> Syntax.value Env.StringMap.t * Syntax.value
val check_test :
  (Type.type_scheme Env.StringMap.t * Env.StringSet.t) *
  Syntax.value Env.StringMap.t ->
  int * int -> Loc.loc * Syntax.def -> int * int
val is_test : 'a * Syntax.def -> bool
val do_def :
  bool ->
  (Type.type_scheme Env.StringMap.t * Env.StringSet.t) *
  Syntax.value Env.StringMap.t ->
  Loc.loc * Syntax.def ->
  (Type.type_scheme Env.StringMap.t * Env.StringSet.t) *
  Syntax.value Env.StringMap.t
val use_func :
  (Type.type_scheme Env.StringMap.t * Env.StringSet.t) *
  Syntax.value Env.StringMap.t ->
  string ->
  Sexpr.expr list ->
  (Type.type_scheme Env.StringMap.t * Env.StringSet.t) *
  Syntax.value Env.StringMap.t
val repl_func :
  (Type.type_scheme Env.StringMap.t * Env.StringSet.t) *
  Syntax.value Env.StringMap.t ->
  Sexpr.expr list ->
  (Type.type_scheme Env.StringMap.t * Env.StringSet.t) *
  Syntax.value Env.StringMap.t
