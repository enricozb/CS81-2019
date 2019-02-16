open Loc

module StringSet = Set.Make(String)

let error = Error.syntax_err

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
  | Ast.TyForAll (tyvars, ty) ->
      Forall (tyvars, ast_type_to_syntax ty)

and ast_types_to_syntax tyvars = List.map ast_type_to_syntax tyvars


let rec ast_to_expr = function
  | Ast.Name (l, id) ->
      Var (l, id)
  | Ast.Num (l, i) ->
      Literal (l, i)
  | Ast.Call (l, expr, args) ->
      Call (l, ast_to_expr expr, List.map ast_to_expr args)
  | Ast.Instantiation (l, expr, type_params) ->
        Narrow (l, ast_to_expr expr, ast_types_to_syntax type_params)
  | Ast.Lambda (l, typed_params, stmt) ->
      let typed_params =
        List.map
          (fun (name, ty) -> (name, ast_type_to_syntax ty))
          typed_params
      in
      Lambda (l, typed_params, ast_to_expr stmt)
  | Ast.TypeLambda (l, typed_params, stmt) ->
      TypeLambda (l, typed_params, ast_to_expr stmt)
  | Ast.If (l, expr, true_stmts, false_stmts) ->
      If (l, ast_to_expr expr,
             ast_list_to_begin true_stmts,
             ast_list_to_begin false_stmts)
  | Ast.While (l, expr, stmts) ->
      While (l, ast_to_expr expr, ast_list_to_begin stmts)
  | Ast.Bind (l, name, expr) ->
      Set (l, name, ast_to_expr expr)
  | Ast.Def _ ->
      failwith "ast_to_expr called on Ast.Def"
  | Ast.Import _ ->
      failwith "ast_to_expr called on Ast.Import"
  | _ ->
      failwith "ast_to_expr called on Ast. something"

and ast_list_to_begin (stmts : Ast.ast list) =
  let begin_loc =
    Loc.span
      (Ast.loc_of_ast @@ List.nth stmts 0)
      (Ast.loc_of_ast @@ List.nth stmts ((List.length stmts) - 1))
  in
  Begin (begin_loc, List.map ast_to_expr stmts)

let ast_to_def = function
  | Ast.Import (l, name) -> Use (l, name ^ ".my1")
  | Ast.Def (l, name, type_vars, typed_params, rtype, stmts) ->
      let rtype = ast_type_to_syntax rtype in
      let typed_params =
        List.map
          (fun (name, ty) -> (name, ast_type_to_syntax ty))
          typed_params
      in
      let param_types = List.map snd typed_params in
      (* If function uses generics, use Forall and TypeLambda *)
      if type_vars <> [] then
        Valrec (l, name, Forall (type_vars, FunctionType (param_types, rtype)),
          TypeLambda (l, type_vars, Lambda (l, typed_params,
                      ast_list_to_begin stmts)))
      else
        Valrec (l, name, FunctionType (param_types, rtype),
          Lambda (l, typed_params, ast_list_to_begin stmts))
  | Ast.Bind (l, name, expr) ->
      Val (l, name, ast_to_expr expr)

  | Ast.CheckExpect (l, expr_1, expr_2) ->
      CheckExpect (l, ast_to_expr expr_1, ast_to_expr expr_2)
  | Ast.CheckType (l, expr, ty) ->
      CheckType (l, ast_to_expr expr, ast_type_to_syntax ty)
  | Ast.CheckError (l, expr) ->
      CheckError (l, ast_to_expr expr)
  | Ast.CheckTypeError (l, expr) ->
      CheckTypeError (l, ast_to_expr expr)
  | ast ->
      Expr (Ast.loc_of_ast ast, ast_to_expr ast)

