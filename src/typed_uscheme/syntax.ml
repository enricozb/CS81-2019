open Loc
open Sexpr

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
  "#t";
  "#f";
  "#u";
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

let get_formal_name expr =
  match expr with
    | Id (l, name) ->
      let name' = validate_name l name in name'
    | _ -> error (Sexpr.loc_of_expr expr) "invalid_formal_parameter"

let get_binding_name expr =
  match expr with
    | List (_, [Id (l, name); _]) ->
      let name' = validate_name l name in name'
    | _ -> error (Sexpr.loc_of_expr expr) "invalid binding"

(* ---------------------------------------------------------------------- *)

(*
 * Main parsing functions.
 * TYPED uSCHEME  (DELETEME)
 *)

let rec parse_expr = function
  | Int (l, i) -> Literal (l, i)

  | Id (l, s) -> Var (l, s)

  | List (l, [Id (_, "set"); Id (_, name); e]) ->
    let name' = validate_name l name in
      Set (l, name', parse_expr e)
  | List (l, Id (_, "set")::_) -> error l "invalid \"set\""

  | List (l, [Id (_, "if"); e1; e2; e3]) ->
    If (l, parse_expr e1, parse_expr e2, parse_expr e3)
  | List (l, Id (_, "if")::_) -> error l "invalid \"if\""

  | List (l, [Id (_, "while"); e1; e2]) ->
    While (l, parse_expr e1, parse_expr e2)
  | List (l, Id (_, "while")::_) -> error l "invalid \"while\""

  | List (l, Id (_, "begin") :: es) -> Begin (l, List.map parse_expr es)

  | List (l, [Id (_, "let"); List (_, bindings); e]) ->
    Let (l, List.map parse_binding bindings, parse_expr e)
  | List (l, Id (_, "let") :: _) -> error l "invalid \"let\""

  | List (l, [Id (_, "let*"); List (_, bindings); e]) ->
    LetStar (l, List.map parse_binding bindings, parse_expr e)
  | List (l, Id (_, "let*") :: _) -> error l "invalid \"let*\""

  | List (l, [Id (_, "lambda"); List (_, formals); e]) ->
    Lambda (l, List.map parse_formal formals, parse_expr e)
  | List (l, Id (_, "lambda") :: _) -> error l "invalid \"lambda\""

  | List (l, Id (_, "@") ::  e :: tys) ->
    Narrow (l, parse_expr e, List.map parse_type tys)
  | List (l, Id (_, "@") :: _) -> error l "invalid \"@\" form"

  | List (l, [Id (_, "type-lambda"); List (_, vars); e]) ->
    TypeLambda (l, List.map parse_tyvar vars, parse_expr e)
  | List (l, Id (_, "type-lambda") :: _) -> error l "invalid \"type-lambda\""

  | List (l, func :: es) -> Call (l, parse_expr func, List.map parse_expr es)

  | List (l, _) -> error l "unrecognized form"

and parse_binding = function
  | List (_, [Id (_, var); expr]) -> (var, parse_expr expr)
  | List (l, _)
  | Id   (l, _)
  | Int  (l, _) ->
    error l "invalid binding"

and parse_formal = function
  | List (l, [Id (_, name); Id (_, ":"); ty]) -> (name, parse_type ty)
  | List (l, _)
  | Id   (l, _)
  | Int  (l, _) ->
    error l "bad formal parameter"

and parse_tyvar = function
  | List (l, [Id (_, "quote"); Id (_, name)]) -> name
  | List (l, _)
  | Id   (l, _)
  | Int  (l, _) ->
    error l "bad type variable"

and is_fun_ty l =
  let len = List.length l in
    len > 1 &&
    match List.nth l (len - 2) with
      | Id (_, "->") -> true
      | _ -> false

and parse_type = function
  | List (_, [Id (_, "quote"); Id (_, name)]) -> TyVar name
  | Id (_, name) -> TyCon name
  | List (l, [Id (_, "forall"); List (_, tyvars); exp]) ->
    Forall (List.map parse_tyvar tyvars, parse_type exp)
  | List (l, Id (_, "forall") :: _) -> error l "bad forall"
  | List (l, call) when is_fun_ty call ->
    (* This is ugly, but looking at the end of a list is always a little
     * unnatural... *)
    begin match List.rev call with
      | retty :: _ :: argtys ->
        FunctionType (List.rev_map parse_type argtys, parse_type retty)
      | _ -> failwith "impossible"
    end
  | List (l, tycon :: args) ->
    TyApp (parse_type tycon, List.map parse_type args)
  | List (l, _) | Int (l, _) -> error l "invalid type"

let parse_def = function
  | List (l, [Id (_, "val"); Id (_, name); init]) ->
    Val (l, name, parse_expr init)

  | List (l, [Id (_, "val-rec"); ty; Id (_, name); init]) ->
    Valrec (l, name, parse_type ty, parse_expr init)

  | List (l, [Id (_, "define"); ty; Id (_, name); List (_, args); body]) ->
    let formals = List.map parse_formal args in
    let body    = parse_expr body in
    let ret     = parse_type ty in
      Define (l, name, ret, formals, body)
  | List (l, Id (_, "define") :: _) -> error l "invalid \"define\""

  | List (l, [Id (_, "use"); Id (_, filename)]) -> Use (l, filename)
  | List (l, Id (_, "use") :: _) -> error l "invalid \"use\""

  | List (l, [Id (_, "check-expect"); to_check; result]) ->
    CheckExpect (l, parse_expr to_check, parse_expr result)
  | List (l, Id (_, "check-expect") :: _) -> error l "invalid \"check-expect\""

  | List (l, [Id (_, "check-error"); to_check]) ->
    CheckError (l, parse_expr to_check)
  | List (l, Id (_, "check-error") :: _) -> error l "invalid \"check-error\""

  | List (l, [Id (_, "check-type"); expr; ty]) ->
    CheckType (l, parse_expr expr, parse_type ty)
  | List (l, Id (_, "check-type") :: _) -> error l "invalid \"check-type\""

  | List (l, [Id (_, "check-type-error"); expr]) ->
    CheckTypeError (l, parse_expr expr)
  | List (l, Id (_, "check-type-error") :: _) ->
      error l "invalid \"check-type-error\""

  (* Top-level expressions. *)
  | other ->
    let expr = parse_expr other in
    let l = loc_of_expr expr in
      Expr (l, expr)

