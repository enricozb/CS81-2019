type expr =
  | Int   of Loc.loc * int
  | Id    of Loc.loc * string
  | List  of Loc.loc * expr list

(** Retrieve the location info from an S-expression. *)
val loc_of_expr : expr -> Loc.loc

(** Convert an S-expression to a string. *)
val string_of_expr : expr -> string

(** Convert an S-expression to a string, with location information. *)
val string_of_expr_loc : expr -> string
