type name = string

type frame =
  | Bind of Loc.loc * name
  | If of Loc.loc * Ast.ast list * Ast.ast list
  | CallEnv of Value.value Env.env
  | List of Loc.loc * Value.value list * Ast.ast list
  | Apply of Loc.loc * Value.value list * Ast.ast list
  | Suite of Loc.loc * Ast.ast list
  | Return of Loc.loc

