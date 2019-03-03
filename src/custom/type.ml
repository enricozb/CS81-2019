(* ---- TYPES ---- *)
type id = string
type ty =
  | TyVar of tyvar ref
  | TyCon of id * (ty list)
  | TyFun of (ty list) * ty

and tyvar =
  | Link of ty
  | Unbound of id

let rec string_of_type = function
  | TyVar {contents = Unbound id} -> id
  | TyVar {contents = Link ty} -> string_of_type ty
  | TyCon (id, []) -> id
  | TyCon (id, param_tys) ->
      (* TODO : implement this garbage *)
      id ^ "<" ^ string_of_type_list param_tys ^ ">"
  | TyFun (param_tys, ret_ty) ->
      "{" ^ (string_of_type_list param_tys) ^ "} -> " ^ (string_of_type ret_ty)

and string_of_type_list = function
  | [] -> ""
  | [ty] -> string_of_type ty
  | ty :: tys ->
    (string_of_type ty) ^ ", " ^ (string_of_type_list tys)

let rec mem_free_var ty_var ty = match ty with
  | TyVar {contents = Unbound y} ->
      ty_var = y
  | TyVar {contents = Link ty} ->
      mem_free_var ty_var ty
  | TyCon (_, param_tys) ->
      List.exists (mem_free_var ty_var) param_tys
  | TyFun (param_tys, ret_ty) ->
      List.exists (mem_free_var ty_var) param_tys || (mem_free_var ty_var ret_ty)

let rec fresh_counter = ref 0
and fresh_tyvar _ =
  fresh_counter := !fresh_counter + 1;
  TyVar {contents = Unbound ("T" ^ string_of_int !fresh_counter)}

(* common types *)
let int_ty = TyCon ("Int", [])
let list_ty elem_ty = TyCon ("List", [elem_ty])
let fun_ty param_tys ret_ty = TyFun (param_tys, ret_ty)

(* ---- SUBSTITUTIONS ---- *)
let rec unify ty1 ty2 =
  if ty1 == ty2 then
    ()
  else
    match (ty1, ty2) with
    | TyCon (name1, ty_params1), TyCon (name2, ty_params2) ->
        if name1 = name2 then
          List.iter2 unify ty_params1 ty_params2
        else
          failwith ("Cannot unify " ^ name1 ^ " and " ^ name2)

    | TyVar {contents = Link ty1}, ty2
    | ty1, TyVar {contents = Link ty2} ->
        unify ty1 ty2

    | TyVar ({contents = Unbound ty_var} as ty_ref), ty
    | ty, TyVar ({contents = Unbound ty_var} as ty_ref) ->
        if not (mem_free_var ty_var ty) then
          ty_ref := Link ty
        else
          failwith (
            "Cannot unify types " ^ (string_of_type ty) ^
            " and " ^ (string_of_type (TyVar ty_ref)) ^ ".")


    | TyFun (ty_params1, ty_ret1), TyFun (ty_params2, ty_ret2) ->
        (* return types should be equal, as should argument types *)
        List.iter2 unify ty_params1 ty_params2;
        unify ty_ret1 ty_ret2
    | (ty1, ty2) ->
        failwith (
          "Cannot unify types " ^ (string_of_type ty1) ^
          " and " ^ (string_of_type ty2) ^ ".")

and pairwise_unify tys = match tys with
  | [] -> ()
  | [ty] -> ()
  | ty1 :: ty2 :: tys ->
      unify ty1 ty2;
      pairwise_unify (ty2 :: tys)

(* ---- TYPE CHECK ---- *)
let rec typecheck ty_env ast =
  let ty = infer ty_env ast in

  match ast with
    | Ast.Bind (l, id, _) ->
        (Env.bind id ty ty_env, ty)
    | _ ->
        (Env.bind "_" ty ty_env, ty)

and infer ty_env ast = match ast with
  | Ast.Name (l, id) -> Env.lookup l id ty_env

  | Ast.Num (l, i) -> int_ty

  | Ast.List (l, asts) ->
      let elem_ty = fresh_tyvar () in
      let tys = List.map (infer ty_env) asts in
      pairwise_unify tys;

      (* unify elem_ty with the first element of the list (if there is one) *)
      if tys != [] then
        unify elem_ty (List.hd tys);

      list_ty elem_ty

  | Ast.Lambda (l, param_names, ast) ->
      let param_tys = List.map fresh_tyvar param_names in
      let ty_env' = Env.bind_many param_names param_tys ty_env in
      let ret_ty = infer ty_env' ast in
      fun_ty param_tys ret_ty

  | Ast.Bind (l, id, ast) -> infer ty_env ast

  | Ast.Call (l, ast, param_asts) ->
      let t1 = infer ty_env ast in
      let param_tys = List.map (infer ty_env) param_asts in
      let ret_ty = fresh_tyvar () in

      (* equate type of function with (tys -> ret_ty) *)
      unify t1 (fun_ty param_tys ret_ty);
      ret_ty

  | _ -> failwith "Type.infer called on non-implemented Ast"

