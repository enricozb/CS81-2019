open Loc

module StringSet = Set.Make(String)

let error = Error.syntax_err

let keywords = [
  "val";
  "val-rec";
  "define";
  "use";
  "check-expect";
  "check-error";
  "check-type";
  "check-type-error";
  "set";
  "if";
  "while";
  "begin";
  "let";
  "let*";
  "lambda";
  "type-lambda"
]

let reserved_ids = [
  "true";
  "false";
  "unit";
  "nil"
]

type id = string

type scheme_type =
  | TyCon        of id
  | TyVar        of id
  | Forall       of id list * scheme_type
  | FunctionType of scheme_type list * scheme_type
  | TyApp        of scheme_type * scheme_type list

type formal = id * scheme_type

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

let loc_of_expr = function
  | Literal    (l, _)
  | Var        (l, _)
  | Set        (l, _, _)
  | If         (l, _, _, _)
  | While      (l, _, _)
  | Begin      (l, _)
  | Let        (l, _, _)
  | LetStar    (l, _, _)
  | Lambda     (l, _, _)
  | Call       (l, _, _)
  | Narrow     (l, _, _)
  | TypeLambda (l, _, _)
    -> l

let loc_of_def = function
  | Val             (l, _, _)
  | Valrec          (l, _, _, _)
  | Define          (l, _, _, _, _)
  | Expr            (l, _)
  | Use             (l, _)
  | CheckExpect     (l, _, _)
  | CheckError      (l, _)
  | CheckType       (l, _, _)
  | CheckTypeError  (l, _)
    -> l

let rec ast_type_to_syntax = function
  | Ast.TyVar s -> TyVar s
  | Ast.TyStr s -> TyCon s
  (*
   * TyApp in concrete syntax only supports type constructors, so no higher
   * order application
   *)
  | Ast.TyApp (s, tys) ->
      TyApp (TyCon s, ast_types_to_syntax tys)
  | Ast.TyFun (tys, rtype) ->
      FunctionType (ast_types_to_syntax tys, ast_type_to_syntax rtype)

and ast_types_to_syntax tyvars = List.map ast_type_to_syntax tyvars

let syntax_tys_to_id_list type_vars =
  let rec iter = function
    | [] -> []
    | (Ast.TyVar s) :: rest -> s :: iter rest
  in iter type_vars


let rec ast_to_expr = function
  | Ast.Name (l, id) ->
      Var (l, id)
  | Ast.Num (l, i) ->
      Literal (l, i)
  | Ast.Call (l, name, args) ->
      Call (l, Var (l, name), List.map ast_to_expr args)
  | Ast.InstantiatedCall (l, name, type_params, args) ->
      Call (l,
        Narrow (l,
          Var (l, name),
          ast_types_to_syntax type_params),
        List.map ast_to_expr args)
  | Ast.Lambda (l, typed_params, stmt) ->
      let typed_params =
        List.map
          (fun (name, ty) -> (name, ast_type_to_syntax ty))
          typed_params
      in
      Lambda (l, typed_params, ast_to_expr stmt)
  | Ast.Bind (l, name, expr) ->
      Set (l, name, ast_to_expr expr)
  | Ast.Def _ ->
      failwith "ast_to_expr called on Ast.Def"


let ast_to_def = function
  | Ast.Def (l, name, type_vars, typed_params, rtype, stmts) ->
      let rtype = ast_type_to_syntax rtype in
      let typed_params =
        List.map
          (fun (name, ty) -> (name, ast_type_to_syntax ty))
          typed_params
      in
      let param_types = List.map snd typed_params in
      (* Location spanning the expressions in the function *)
      let begin_loc =
        Loc.span
          (Ast.loc_of_ast @@ List.nth stmts 0)
          (Ast.loc_of_ast @@ List.nth stmts ((List.length stmts) - 1))
          in
      (* If function uses generics, use Forall and TypeLambda *)
      if type_vars <> [] then
        Valrec (l, name, Forall (type_vars, FunctionType (param_types, rtype)),
          TypeLambda (l, type_vars, Lambda (l, typed_params,
              Begin (begin_loc, List.map ast_to_expr stmts))))
      else
        Valrec (l, name, FunctionType (param_types, rtype),
          Lambda (l, typed_params,
            Begin (begin_loc, List.map ast_to_expr stmts)))

  | Ast.Bind (l, name, expr) ->
      Val (l, name, ast_to_expr expr)
  | ast ->
      Expr (Ast.loc_of_ast ast, ast_to_expr ast)



(* ---------------------------------------------------------------------- *)

(*
 * Helper functions for syntax analysis.
 *)

let unique_ids ids =
  StringSet.cardinal (StringSet.of_list ids) = List.length ids

let not_keyword loc name =
  if List.mem name keywords
  then error loc ("keywords can't be variable/function names: " ^ name)
  else name

let not_reserved loc (name : string) : string =
  if List.mem name reserved_ids
  then error loc ("reserved names can't be variable/function names: " ^ name)
  else name

let validate_name loc name =
  let name'  = not_keyword loc name in
  let name'' = not_reserved loc name' in
    name''

