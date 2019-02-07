(*
 * Representing and analyzing Scheme syntax.
 *)

open Loc

(* Identifiers. *)
type id = string

(* Scheme expressions. *)
type expr =
  | Literal    of loc * int
  | Var        of loc * id
  | Set        of loc * id * expr
  | If         of loc * expr * expr * expr
  | While      of loc * expr * expr
  | Begin      of loc * expr list
  | Let        of loc * (id * expr) list * expr
  | LetStar    of loc * (id * expr) list * expr
  | Lambda     of loc * formal list * expr
  | Call       of loc * expr * expr list
  | Narrow     of loc * expr * scheme_type list
  | TypeLambda of loc * id list * expr

and formal = id * scheme_type

and scheme_type =
  (* Type constructors like `int` and `list`. *)
  | TyCon of id
  (* Type variables, including the `'`. *)
  | TyVar of id
  (* Forall types, with a list of type variables and a scheme type. *)
  | Forall of id list * scheme_type
  | FunctionType of scheme_type list * scheme_type
  (* Type application, e.g. `list int`. *)
  | TyApp of scheme_type * scheme_type list

(* Scheme top-level forms. *)
type def =
  | Val            of loc * id * expr
  | Valrec         of loc * id * scheme_type * expr
  | Define         of loc * id * scheme_type * formal list * expr
  | Expr           of loc * expr
  | Use            of loc * id
  | CheckExpect    of loc * expr * expr
  | CheckError     of loc * expr
  | CheckType      of loc * expr * scheme_type
  | CheckTypeError of loc * expr

val loc_of_expr : expr -> loc
val loc_of_def  : def -> loc
val ast_to_def : Ast.ast -> def

