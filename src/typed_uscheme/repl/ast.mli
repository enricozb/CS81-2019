type ty =
  | TyVar of string
  | TyStr of string
  | TyApp of string * ty list
  | TyFun of (ty list) * ty
  | TyForAll of (string list) * ty

type name = string
type tyvar = string
type typed_namelist = (name * ty) list

type ast =
  | Name of Loc.loc * name
  | Num of Loc.loc * int
  | Lambda of (Loc.loc * typed_namelist * ast)
  | Call of (Loc.loc * ast * (ast list))
  | Instantiation of (Loc.loc * ast * (ty list))
  | Bind of (Loc.loc * name * ast)
  | If of (Loc.loc * ast * (ast list) * (ast list))
  | While of (Loc.loc * ast * (ast list))
  | Def of (Loc.loc * name * (tyvar list) * (typed_namelist) * ty * (ast list))

val loc_of_ast : ast -> Loc.loc
val loc_of_ast_list : ast list -> Loc.loc

