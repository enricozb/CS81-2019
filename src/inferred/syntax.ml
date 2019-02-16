open Sexpr
open Env
open Type
open Error

let error = Error.syntax_err

type expr = Literal of value
          | Var of string
          | If of annotated_expr * annotated_expr * annotated_expr
          | Begin of annotated_expr list
          | Call of annotated_expr * annotated_expr list
          | LetX of let_kind * (string * annotated_expr) list * annotated_expr
          | Lambda of lambda

and let_kind = Let | LetRec | LetStar

and lambda = string list * annotated_expr

and annotated_expr = Loc.loc * expr

and value = Nil
          | Bool of bool
          | Num of int
          | Sym of string
          | Pair of value * value
          | Closure of lambda * (unit -> value env)
          | Primitive of primop
          | Ref of value ref

and primop = value list -> Loc.loc -> value

let eq_val a b = match (a, b) with
    | (Nil, Nil) -> true
    | (Num a, Num b) -> a = b
    | (Bool a, Bool b) -> a = b
    | (Sym a, Sym b) -> a = b
    (* TODO: Handle pair, closure, etc. in error? *)
    | _ -> false

let rec render_val = function
  | Nil -> "'()"
  | Bool b -> if b then "#t" else "#f"
  | Num i -> string_of_int i
  | Sym s -> s
   (* How should we render pairs versus lists? *)
  | Pair (a, b) ->
    "'(" ^ render_pair a b ^ ")"
  | Closure _ -> "<closure>"
  | Primitive _ -> "<primitive>"
  | Ref r -> "ref " ^ (render_val !r)

and render_pair a =
  let a = render_val a in function
    | Nil -> a
    | Pair (b, c) -> a ^ " " ^ render_pair b c
    | other -> a ^ " . " ^ render_val other

type def = Val of string * annotated_expr
         | ValRec of string * annotated_expr
         | Expr of annotated_expr
         | Define of string * string list * annotated_expr
         | Use of string
         | CheckExpect of annotated_expr * annotated_expr
         | CheckError of annotated_expr
         | CheckType of annotated_expr * type_scheme
         | CheckPrincipalType of annotated_expr * type_scheme
         | CheckTypeError of annotated_expr

and annotated_def = Loc.loc * def

let rec parse_expr = function
  | List (l, [Id (_, "quote"); List (_, [])]) -> (l, Literal Nil)
  | List (l, [Id (_, "quote"); Id (_, s)]) -> (l, Literal (Sym s))

  (* A quoted sexpr is default assumed to be a list in type-checking. *)
  | List (l, [Id (_, "quote"); List (_, symbols)]) ->
    let symbols = List.map parse_list_literal symbols in
      (l, Literal (List.fold_right (fun a b -> Pair (a, b)) symbols Nil))
  
(*
  | Quote (l, _) -> error l "invalid \"quote\""
*)

  | Id (l, var) -> (l, Var var)

  | Int (l, i) -> (l, Literal (Num i))

  | List (l, [Id (_, "if"); e1; e2; e3]) ->
    (l, If (parse_expr e1, parse_expr e2, parse_expr e3))
  | List (l, Id (_, "if")::_) -> error l "invalid \"if\""

  | List (l, Id (_, "begin") :: es) -> (l, Begin (List.map parse_expr es))

  | List (l, [Id (_, "let"); List (_, bindings); e]) ->
    (l, LetX (Let, List.map parse_binding bindings, parse_expr e))
  | List (l, Id (_, "let") :: _) -> error l "invalid \"let\""

  | List (l, [Id (_, "let*"); List (_, bindings); e]) ->
    (l, LetX (LetStar, List.map parse_binding bindings, parse_expr e))
  | List (l, Id (_, "let*") :: _) -> error l "invalid \"let*\""

  | List (l, [Id (_, "letrec"); List (_, bindings); e]) ->
    (l, LetX (LetRec, List.map parse_binding bindings, parse_expr e))
  | List (l, Id (_, "letrec") :: _) -> error l "invalid \"letrec\""

  | List (l, [Id (_, "lambda"); List (_, formals); e]) ->
    (l, Lambda (List.map parse_formal formals, parse_expr e))
  | List (l, Id (_, "lambda") :: _) -> error l "invalid \"lambda\""

  | List (l, func :: params) ->
    (l, Call (parse_expr func, List.map parse_expr params))

  | List (l, _) -> error l "unrecognized form"

and parse_list_literal = function
  | Id (l, s) -> Sym s
  | Int (l, n) -> Num n

    (* Sub-lists are okay too. *)
  | List(l, symbols) ->
    let symbols = List.map parse_list_literal symbols in
      (List.fold_right (fun a b -> Pair (a, b)) symbols Nil)

(*
  | Quote (l, _) -> error l "Invalid symbol in literal list"
*)

and parse_binding = function
  | List (_, [Id (_, name); expr]) -> (name, parse_expr expr)
  | List (l, _) | Id (l, _) | Int (l, _) (* | Quote (l, _) *) -> error l "invalid binding"

and parse_formal = function
  | Id (l, name) -> name
  | List (l, _) |  Int (l, _) (* | Quote (l, _) *) -> error l "invalid formal"

let is_fun_ty l =
  let len = List.length l in
  len > 1 &&
  match List.nth l (len - 2) with
  | Id (_, "->") -> true
  | _ -> false

let rec parse_type = function
  | List (_, [Id (_, "quote"); Id (_, a)]) -> TyVar a
  (* | Quote (l, Id (_, a)) -> TyVar a *)
  (* | Quote (l, _) -> error l "invalid type variable" *)
  | List (l, (Id (_, "quote") :: _)) -> error l "invalid type variable"
  | Id (l, a) -> TyCon a

  | List (l, Id (_, "forall") :: _) ->
        error l "invalid forall as type constructor"

  | List (l, call) when is_fun_ty call ->
    (* This is ugly, but looking at the end of a list is always a little
     * unnatural... *)
    begin match List.rev call with
      | [retty; _; List(_, [])] ->
        funtype_of [] (parse_type retty)
      | retty :: _ :: argtys ->
        funtype_of (List.rev_map parse_type argtys) (parse_type retty)
      | _ -> raise NanoML_NeverHappen_err
    end

  | List (l, tycon :: args) ->
    TyApp (parse_type tycon, List.map parse_type args)

  | List (l, _) | Int (l, _) -> error l "invalid type"

let parse_arg = function
  (* | Quote (l, Id (_, a)) -> a *)
  | List (l, [Id (_, "quote"); Id (_, a)]) -> a
  | List (l, _) | Int (l, _) (* | Quote (l, _) *) | Id (l, _) ->
    error l "invalid type variable"

let parse_args = function
  (* | Quote (l, Id (_, a)) -> [a] *)
  | List (l, [Id (_, "quote"); Id (_, a)]) -> [a]
  | List (l, args) -> List.map parse_arg args
  | Int (l, _) (* | Quote (l, _) *) | Id (l, _) -> error l "invalid type variable"

let parse_tyscheme = function
  | List (l, [Id (_, "forall"); args; ty]) ->
    ForAll (parse_args args, parse_type ty)
  | t -> ForAll ([], parse_type t)

let parse_def = function
  | List (l, [Id (_, "val"); Id (_, name); expr]) ->
    (l, Val (name, parse_expr expr))
  | List (l, Id (_, "val") :: _) -> error l "Invalid \"val\""

  | List (l, [Id (_, "valrec"); Id (_, name); expr]) ->
    (l, ValRec (name, parse_expr expr))
  | List (l, Id (_, "valrec") :: _) -> error l "Invalid \"valrec\""

  | List (l, [Id (_, "define"); Id(_, name); List (_, formals); expr]) ->
    (l, Define (name, List.map parse_formal formals, parse_expr expr))
  | List (l, Id (_, "define") :: _) -> error l "invalid \"define\""

  | List (l, [Id (_, "check-expect"); a; b]) ->
    (l, CheckExpect (parse_expr a, parse_expr b))
  | List (l, Id (_, "check-expect") :: _) -> error l "invalid \"check-expect\""

  | List (l, [Id (_, "check-error"); a]) ->
    (l, CheckError (parse_expr a))
  | List (l, Id (_, "check-error") :: _) -> error l "invalid \"check-error\""

  | List (l, [Id (_, "check-type"); a; b]) ->
    (l, CheckType (parse_expr a, parse_tyscheme b))
  | List (l, Id (_, "check-type") :: _) -> error l "invalid \"check-type\""

  | List (l, [Id (_, "check-principal-type"); a; b]) ->
    (l, CheckPrincipalType (parse_expr a, parse_tyscheme b))
  | List (l, Id (_, "check-principal-type") :: _) -> error l "invalid \"check-principal-type\""

  | List (l, [Id (_, "check-type-error"); a]) ->
    (l, CheckTypeError (parse_expr a))
  | List (l, Id (_, "check-type-error") :: _) -> error l "invalid \"check-type-error\""

  | List (l, [Id (_, "use"); Id (_, filename)]) ->
    (l, Use filename)
  | List (l, Id (_, "use") :: _) -> error l "invalid \"use\""

  | other -> let (l, expr) = parse_expr other in
    (l, Expr (l, expr))
