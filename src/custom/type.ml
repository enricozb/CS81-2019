(* ---- TYPES ---- *)
type id = string
type ty =
  | TyVar of id
  | TyCon of id * (ty list)
  | TyFun of (ty list) * ty

type constraints = (ty * ty) list

let rec string_of_type = function
  | TyVar id -> id
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
  | TyVar y ->
      ty_var = y
  | TyCon (_, param_tys) ->
      List.exists (mem_free_var ty_var) param_tys
  | TyFun (param_tys, ret_ty) ->
      List.exists (mem_free_var ty_var) param_tys || (mem_free_var ty_var ret_ty)

let rec fresh_counter = ref 0
and fresh_tyvar _ =
  (fresh_counter := !fresh_counter + 1;
  TyVar ("T" ^ string_of_int !fresh_counter))

(* common types *)
let int_ty = TyCon ("Int", [])
let list_ty elem_ty = TyCon ("List", [elem_ty])
let fun_ty param_tys ret_ty = TyFun (param_tys, ret_ty)

(* ---- SUBSTITUTIONS ---- *)
let (|->) ty_var ty = Env.bind ty_var ty Env.empty

let subst_var subst ty_var =
  if Env.mem ty_var subst then
    Env.get ty_var subst
  else
    TyVar ty_var

(* TODO : potentially change `subst` to `sigma` *)
let rec subst_ty subst ty = match ty with
  | TyVar x -> subst_var subst x
  | TyCon (id, param_tys) ->
      TyCon (id, List.map (subst_ty subst) param_tys)
  | TyFun (param_tys, ret_ty) ->
      TyFun (List.map (subst_ty subst) param_tys, subst_ty subst ret_ty)

let subst_cons subst cons = match cons with
  | [] -> []
  | (ty1, ty2) :: cons -> (subst_ty subst ty1, subst_ty subst ty2) :: cons

let subst_compose subst2 subst1 : ty Env.env =
  let domain = Env.Dom.union (Env.dom subst1) (Env.dom subst2) in
  let replace ty_var = subst_ty subst2 (subst_var subst1 ty_var) in
    Env.Dom.fold
      (fun ty_var subst -> Env.bind ty_var (replace ty_var) subst)
      domain Env.empty

let rec print_subst subst =
  Env.StringMap.iter
    (fun ty_var ty -> Printf.printf "\t%s |-> %s\n" ty_var (string_of_type ty))
    subst

(* ---- CONSTRAINTS ---- *)
(* TODO : check that tys1 and tys2 have the same length *)
let cons_zipeq tys1 tys2 = List.combine tys1 tys2

let rec cons_alleq tys = match tys with
  | [] -> []
  | [ty] -> []
  | ty1 :: ty2 :: tys -> (ty1, ty2) :: (cons_alleq (ty2 :: tys))

let rec string_of_cons cons = match cons with
  | [] -> ""
  | [(ty1, ty2)] -> (string_of_type ty1) ^ " = " ^ (string_of_type ty2)
  | (ty1, ty2) :: cons ->
      (string_of_type ty1) ^ " = " ^ (string_of_type ty2) ^ ", " ^
      (string_of_cons cons)

let rec unify cons =
  Printf.printf "%s\n" (string_of_cons cons);

  match cons with
  | [] -> Env.empty
  | (TyVar x, ty) :: cons
  | (ty, TyVar x) :: cons ->
      if not (mem_free_var x ty) then
        let subst = unify (subst_cons (x |-> ty) cons) in
        subst_compose subst (x |-> ty)
      else
        failwith (
          "Cannot unify types " ^ (string_of_type ty) ^
          " and " ^ (string_of_type (TyVar x)) ^ ".")
  | (TyCon (name1, ty_params1), TyCon (name2, ty_params2)) :: cons ->
      if name1 = name2 then
        unify ((cons_zipeq ty_params1 ty_params2) @ cons)
      else
        failwith ("Cannot unify " ^ name1 ^ " and " ^ name2)
  | (TyFun (ty_args1, ty_ret1), TyFun (ty_args2, ty_ret2)) :: cons ->
      (* return types should be equal, as should argument types *)
      let cons =
        (ty_ret1, ty_ret2) :: ((cons_zipeq ty_args1 ty_args2) @ cons) in
      unify cons
  | (ty1, ty2) :: cons ->
      failwith (
        "Cannot unify types " ^ (string_of_type ty1) ^
        " and " ^ (string_of_type ty2) ^ ".")

(* ---- TYPE CHECK ---- *)
let rec typecheck ty_env ast =
  let (ty, cons) = infer ty_env ast in
  let subst = unify cons in

  print_subst subst;

  let ty = subst_ty subst ty in
  (* check for defs *)
  match ast with
    | Ast.Bind (l, id, _) ->
        (Env.bind id ty ty_env, ty)
    | _ ->
        (Env.bind "_" ty ty_env, ty)

and infer ty_env ast = match ast with
  | Ast.Name (l, id) ->
      (Env.lookup l id ty_env, [])

  | Ast.Num (l, i) -> (int_ty, [])

  | Ast.List (l, asts) ->
      let elem_ty = fresh_tyvar () in
      let (tys, conss) = List.split @@ List.map (infer ty_env) asts in
      let cons = (List.flatten conss) @ (cons_alleq tys) in
      (* ensure the first element of the list (if there is one) equals elem_ty *)
      begin match tys with
        | [] -> (list_ty elem_ty, cons)
        | ty :: _ -> (list_ty elem_ty, (ty, elem_ty) :: cons)
      end

  | Ast.Lambda (l, param_names, ast) ->
      let param_tys = List.map fresh_tyvar param_names in
      let ty_env' = Env.bind_many param_names param_tys ty_env in
      let (ret_ty, cons) = infer ty_env' ast in
      (fun_ty param_tys ret_ty, cons)

  | Ast.Bind (l, id, ast) -> infer ty_env ast

  | Ast.Call (l, ast, param_asts) ->
      let (t1, cons1) = infer ty_env ast in
      let (tys, cons2) = List.split @@ List.map (infer ty_env) param_asts in
      let cons2 = List.flatten cons2 in
      let ret_ty = fresh_tyvar () in
      (* equate type of function with (tys -> ret_ty) *)
      (ret_ty, (t1, fun_ty tys ret_ty) :: (cons1 @ cons2))

  | _ -> failwith "Type.infer called on non-implemented Ast"

