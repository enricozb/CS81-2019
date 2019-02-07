type ty = string
type name = string
type typed_namelist = (name * ty) list

type ast =
  | Name of Loc.loc * name
  | Num of Loc.loc * int
  | Lambda of (Loc.loc * typed_namelist * ast)
  | Call of (Loc.loc * name * (ast list))
  | Bind of (Loc.loc * name * ast)
  | Def of (Loc.loc * name * (ty list) * (typed_namelist) * ty * (ast list))

val loc_of_ast : ast -> Loc.loc
val loc_of_ast_list : ast list -> Loc.loc

