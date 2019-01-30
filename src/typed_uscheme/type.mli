(*
 * Typechecking.
 *)

(* A kind is either a type, or a type constructor. *)
type kind =
  | Type
  | Arrow of kind list * kind

val string_of_type : Syntax.scheme_type -> string

(* Type equality. *)
val (=|=) : Syntax.scheme_type -> Syntax.scheme_type -> bool

(*
 * Type environments.
 *)
type env

val make_env :
  (string * Syntax.scheme_type) list -> (string * kind) list -> env
val bind_kind : env -> string -> kind -> env
val bind_type : env -> string -> Syntax.scheme_type -> env

val typecheck_expr :
  env -> Syntax.expr -> Syntax.scheme_type
val typecheck_val :
  env -> Loc.loc -> string -> Syntax.expr -> Syntax.scheme_type * env
val typecheck_valrec :
  env -> Loc.loc -> string -> Syntax.scheme_type -> Syntax.expr -> env
val typecheck_define :
  env -> Loc.loc -> string -> Syntax.scheme_type -> Syntax.formal list -> Syntax.expr -> env

(*
 * Some type constructors that need special support in the type-checker, and are
 * therefore exposed here.
 *)
val bool_ty : Syntax.scheme_type
val int_ty  : Syntax.scheme_type
val unit_ty : Syntax.scheme_type
