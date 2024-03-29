type name = string

module NameMap : Map.S with type key = name

type ty =
  | TyVar of Loc.loc * name
  | TyCon of Loc.loc * name * (ty list)

type ast =
  | Name of Loc.loc * name
  | Num of Loc.loc * string
  | String of Loc.loc * string
  | List of Loc.loc * (ast list)
  | Record of Loc.loc * (ast NameMap.t)
  | Field of Loc.loc * ast * name
  | SetField of Loc.loc * ast * name * ast
  | Lambda of (Loc.loc * param_list * ast)
  | Call of (Loc.loc * ast * (ast list))
  | Bind of (Loc.loc * bool * name * ast)
  | Assign of (Loc.loc * name * ast)
  | If of (Loc.loc * ast * (ast list) * (ast list))
  | While of (Loc.loc * ast * (ast list))
  | Break of Loc.loc
  | Continue of Loc.loc
  | Def of (Loc.loc * name * param_list * (ty option) * (ast list))
  | Return of (Loc.loc * ast)
  | Class of (Loc.loc * name * (ast list))
  | Suite of (Loc.loc * (ast list)) (* used only outside of parser *)
  | Import of (Loc.loc * name)
  | CheckExpect of (Loc.loc * ast * ast)
  | CheckError of (Loc.loc * ast)
  | CheckType of (Loc.loc * ast)
  | CheckTypeError of (Loc.loc * ast)

and param_list = (name * (ty option)) list

val string_of_ast : ast -> string

val loc_of_ast : ast -> Loc.loc
val loc_of_ast_list : ast list -> Loc.loc
val is_expr : ast -> bool

val compare_names : string -> string -> int

val class_split_suite : ast list -> (ast list) * (ast list)

