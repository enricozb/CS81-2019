type name = string
type mut = bool

type frame =
  | Assign of Loc.loc * name
  | Bind of Loc.loc * mut * name
  | Object of Loc.loc * name * Value.value Value.FieldMap.t * Ast.ast Ast.NameMap.t
  | Field of Loc.loc * name
  | If of Loc.loc * Ast.ast list * Ast.ast list
  | WhileTest of Loc.loc * Ast.ast * Ast.ast list
  | WhileBody of Loc.loc * Ast.ast * Ast.ast list
  | CallEnv of Value.env_value Env.env
  | List of Loc.loc * Value.value list * Ast.ast list
  | Apply of Loc.loc * Value.value list * Ast.ast list
  | Suite of Loc.loc * Ast.ast list
  | Return of Loc.loc

