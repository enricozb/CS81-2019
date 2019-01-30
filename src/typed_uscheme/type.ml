open Syntax

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let rec string_of_type = function
  | TyCon s -> s
  | TyVar s -> "'" ^ s
  | Forall (vs, ty) ->
    let vars = String.concat " " @@ List.map ((^) "'") vs in
    "(forall (" ^ vars ^ ") " ^ string_of_type ty ^ ")"
  | FunctionType (formals, retty) ->
    let formals = List.map string_of_type formals |> String.concat " " in
    "(" ^ formals ^ " -> " ^ string_of_type retty ^ ")"
  | TyApp (con, args) ->
    let types = con :: args |> List.map string_of_type in
    "(" ^ String.concat " " types ^ ")"

let bool_ty = TyCon "bool"
let int_ty  = TyCon "int"
let unit_ty = TyCon "unit"

type kind =
  | Type
  | Arrow of kind list * kind

let rec string_of_kind = function
  | Type -> "*"
  | Arrow (params, result) ->
    string_of_kinds params ^ " -> " ^ string_of_kind result
and string_of_kinds kinds = List.map string_of_kind kinds |> String.concat " "

(* Display kinds, as for error messages. *)
let display_kinds kinds =
  let plural = match kinds with
    | [] -> ""
    | _ -> "s"
  in
  Printf.sprintf "kind%s (%s)" plural @@ string_of_kinds kinds

type env =
  {
    kinds: kind StringMap.t;
    types: scheme_type StringMap.t;
  }

let empty =
  {
    kinds = StringMap.empty;
    types = StringMap.empty;
  }

(* Like `StringMap.find`, but throws a Uscheme_err if not found. *)
let find_err map l name =
  match StringMap.find_opt name map with
  | Some thing -> thing
  | None ->
      Error.name_err l name

let find_kind env l name = find_err env.kinds l name
let bind_kind {kinds; types} name kind =
  let kinds = StringMap.add name kind kinds in
  {kinds; types}

let find_type env l name = find_err env.types l name
let bind_type {kinds; types} name ty =
  let types = StringMap.add name ty types in
  {kinds; types}
let bind_types env formals =
  let add_binding env (v, expr_ty) =
    bind_type env v expr_ty
  in
  List.fold_left add_binding env formals


let make_env types kinds =
  let assoc_list_to_map l =
    List.fold_left (fun m (k, v) -> StringMap.add k v m) StringMap.empty l
  in
  { kinds = assoc_list_to_map kinds; types = assoc_list_to_map types }

(* Get the free type variables in a type. *)
let free_tyvars ty =
  let rec helper bound_tyvars free_tyvars = function
    | TyCon name -> free_tyvars
    | TyVar v ->
      if StringSet.mem v bound_tyvars then
        free_tyvars
      else
        v :: free_tyvars
    | Forall (vars, ty) ->
      let bound_tyvars = StringSet.union (StringSet.of_list vars)
          bound_tyvars in
      helper bound_tyvars free_tyvars ty
    | FunctionType (param_tys, retty) ->
      let free_in_params = List.map (helper bound_tyvars []) param_tys in
      let free_in_retty = helper bound_tyvars [] retty in
      (List.concat (free_in_retty :: free_in_params)) @ free_tyvars
    | TyApp (con, others) ->
      let free_in_con = helper bound_tyvars [] con in
      let free_in_others = List.map (helper bound_tyvars []) others in
      (List.concat (free_in_con :: free_in_others)) @ free_tyvars
  in
  helper StringSet.empty [] ty

let all_eq l1 l2 =
  try
    List.map2 (=) l1 l2 |> List.fold_left (&&) true
  with Invalid_argument _ ->
    false

let rec kind_of env l = function
  | TyVar _ -> Type
  | TyCon t -> find_kind env l t
  | Forall (vars, body) -> Type
  | FunctionType (_, _) -> Type
  | TyApp (con, actuals) ->
    let con_kind = kind_of env l con in
    let actual_kinds = List.map (kind_of env l) actuals in
    begin match con_kind with
      | Arrow (formal_kinds, result_kind) ->
        if not (all_eq formal_kinds actual_kinds) then begin
          let expected =
            Printf.sprintf "%s for constructor %s"
              (display_kinds formal_kinds) (string_of_type con)
          in
          Error.type_err l ~expected ~found:(display_kinds actual_kinds)
        end;
        result_kind
      | Type -> Error.type_err l ~expected:"type constructor"
                  ~found:"type of kind \"*\""
    end

(* Ensure that a type has kind *. *)
let check_simple_type env l ty =
  let kind = kind_of env l ty in
  match kind with
  | Arrow _ ->
      Error.type_err l ~expected:("type of kind \"*\"")
                       ~found:("type of kind \"" ^ string_of_kind kind ^ "\"")
  | _ -> ()

(* A capture-avoiding substitution of the mapping in `tys` over the typed
 * variables in `t`. *)
let rec subst tys = function
  | TyCon name -> TyCon name
  | TyVar v ->
    begin match StringMap.find_opt v tys with
      | Some other -> other
      | None -> TyVar v
    end
  | Forall (vars, body) ->
    (* Forget about captured variables in the body *)
    let tys' = StringMap.filter (fun x _ -> List.mem x vars) tys in
    Forall (vars, subst tys' body)
  | FunctionType (param_tys, retty) ->
    FunctionType (List.map (subst tys) param_tys, subst tys retty)
  | TyApp (tycon, actuals) ->
    TyApp (subst tys tycon, List.map (subst tys) actuals)

let map_of_alist alist =
  let folder m (k, v) = StringMap.add k v m in
  List.fold_left folder StringMap.empty alist

let rec (=|=) ty1 ty2 =
  match (ty1, ty2) with
  | (TyCon con1, TyCon con2) -> con1 = con2
  | (TyVar v1, TyVar v2) -> v1 = v2
  | (Forall (new_tyvars1, body1), Forall (new_tyvars2, body2)) ->
    let new_vars = List.map (fun x -> TyVar x) new_tyvars1 in
    begin try
        let map = map_of_alist @@ List.combine new_tyvars2 new_vars in
        let body2' = subst map body2 in
        body1 =|= body2'
      with
        (Invalid_argument _) -> false
    end
  | (FunctionType (param_tys1, retty1), FunctionType (param_tys2, retty2)) ->
    eq_types param_tys1 param_tys2 && retty1 =|= retty2
  | (TyApp (tycon1, actuals1), TyApp (tycon2, actuals2)) ->
    tycon1 =|= tycon2 && eq_types actuals1 actuals2
  | _ -> false

and eq_types ts1 ts2 =
  try List.combine ts1 ts2
      |> List.map (fun (x, y) -> x =|= y)
      |> List.fold_left (&&) true
  (* Lists were of different lengths. *)
  with Invalid_argument _ -> false

let rec typecheck_expr env expr =
  let assert_eq l t1 t2 =
    if not (t1 =|= t2) then
      Error.type_err l ~expected:(string_of_type t1) ~found:(string_of_type t2)
  in
  let typecheck = typecheck_expr env in
  let add_binding env (v, expr_ty) =
    bind_type env v expr_ty
  in
  match expr with
    | Literal (_, i) -> int_ty

    | Var (l, v) -> find_type env l v

    | Set (l, v, expr) ->
      let v_ty = find_type env l v in
      let e_ty = typecheck expr in
        begin
          assert_eq l v_ty e_ty;
          unit_ty
        end

    | If (l, cond, then_b, else_b) ->
      begin
        assert_eq (Syntax.loc_of_expr cond) (typecheck cond) bool_ty;
        let then_ty = typecheck then_b in
        let else_ty = typecheck else_b in
          begin
            assert_eq l then_ty else_ty;
            then_ty
          end
      end

    | While (_, cond, body) ->
      begin
        assert_eq (Syntax.loc_of_expr cond) (typecheck cond) bool_ty;
        ignore (typecheck body);
        unit_ty
      end

    | Begin (_, exprs) ->
      begin
        match List.rev_map typecheck exprs with
          | last :: rest -> last
          | _ -> unit_ty
      end

    | Let (_, bindings, body) ->
      let bindings = List.map (fun (n, expr) -> (n, typecheck expr)) bindings in
      let env' = List.fold_left add_binding env bindings in
      typecheck_expr env' body

    | LetStar (_, bindings, body) ->
      let folder env (name, expr) =
        add_binding env (name, typecheck_expr env expr)
      in
      let env' = List.fold_left folder env bindings in
      typecheck_expr env' body

    | Lambda (l, formals, expr) ->
      let formal_tys = List.map snd formals in
      List.iter (check_simple_type env l) formal_tys;
      let env' = List.fold_left add_binding env formals in
      let retty = typecheck_expr env' expr in
      FunctionType (formal_tys, retty)

    | Call (l, fn, actuals) ->
      begin
        match typecheck fn with
          | FunctionType (formals, retty) ->
            let actuals = List.map typecheck actuals in
            begin
              try
                List.iter2 (assert_eq l) formals actuals
              (* Number of actual parameters did not match number of formals. *)
              with Invalid_argument _ ->
                let n_args l = Printf.sprintf "%d parameters" (List.length l) in
                  Error.type_err l
                    ~expected:(n_args formals)
                    ~found:(n_args actuals)
            end;
            retty
          | other -> Error.type_err l ~expected:"function type"
                       ~found:(string_of_type other)
      end

    | Narrow (l, e, tys) ->
      List.iter (check_simple_type env l) tys;
      let e_ty = typecheck e in
      begin match e_ty with
        | Forall (formals, t) ->
          begin try
              subst (map_of_alist @@ List.combine formals tys) t
            with Invalid_argument _ ->
              let l1 = string_of_int @@ List.length formals in
              let l2 = string_of_int @@ List.length tys in
              Error.type_err l ~expected:(l1 ^ " types in '@' form")
                ~found:l2
          end
        | _ -> Error.type_err l ~expected:"quantified type"
                 ~found:(string_of_type e_ty)
      end

    | TypeLambda (_, vars, expr) -> Forall (vars, typecheck expr)

let typecheck_define env l name retty formals expr =
  let functype = FunctionType (List.map snd formals, retty) in
  let env' = bind_type env name functype in
  let env'' = bind_types env' formals in
  let expr_ty = typecheck_expr env'' expr in
  if not (retty =|= expr_ty) then begin
    Error.type_err l ~expected:(string_of_type retty) ~found:(string_of_type expr_ty)
  end;
  env'

let typecheck_val env l name expr =
  let expr_ty = typecheck_expr env expr in
  let env' = bind_type env name expr_ty in
  (expr_ty, env')

let typecheck_valrec env l name ty expr =
  let env' = bind_type env name ty in
  let expr_ty = typecheck_expr env' expr in
  if not (ty =|= expr_ty) then begin
    Error.type_err l ~expected:(string_of_type ty) ~found:(string_of_type expr_ty)
  end;
  env'

