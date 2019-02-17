type name = string

type ast =
  | Name of Loc.loc * name
  | Num of Loc.loc * int
  | Lambda of (Loc.loc * (name list) * ast)
  | Call of (Loc.loc * ast * (ast list))
  | Bind of (Loc.loc * name * ast)
  | If of (Loc.loc * ast * (ast list) * (ast list))
  | While of (Loc.loc * ast * (ast list))
  | Def of (Loc.loc * name * (name list) * (ast list))
  | Import of (Loc.loc * name)
  | CheckExpect of (Loc.loc * ast * ast)
  | CheckError of (Loc.loc * ast)
  | CheckTypeError of (Loc.loc * ast)

val loc_of_ast : ast -> Loc.loc
val loc_of_ast_list : ast list -> Loc.loc

