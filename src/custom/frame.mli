type name = string
type mut = bool

type frame =
  | Assign of Loc.loc * name
  | Bind of Loc.loc * mut * name
  | If of Loc.loc * Ast.ast list * Ast.ast list
  | CallEnv of Value.env_value Env.env
  | List of Loc.loc * Value.value list * Ast.ast list
  | Apply of Loc.loc * Value.value list * Ast.ast list
  | Suite of Loc.loc * Ast.ast list
  | Return of Loc.loc

