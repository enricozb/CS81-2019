(* ---- TYPES ---- *)
type id = string
type level = int

type ty =
  | TyVar of tyvar ref
  | TyCon of id * (ty list)
  | TyFun of (ty list) * ty

and tyvar =
  | Link of ty
  | Unbound of id * level
  | Generic of id

let rec string_of_type = function
  | TyVar {contents = Generic id} -> "'" ^ id
  | TyVar {contents = Unbound (id, _)} -> "~" ^ id
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


(* errors if occurs check fails, otherwise returns unit *)
let rec occurs loc tyvar_id tyvar_level ty =
  let recurse = occurs loc tyvar_id tyvar_level in

  match ty with
    | TyVar {contents = Generic _} -> assert false
    | TyVar ({contents = Unbound(tyvar_id2, tyvar_level2)} as tyvar2) ->
      if tyvar_id = tyvar_id2 then
        Error.type_error loc "recursive types"
      else
        if tyvar_level2 > tyvar_level then
          tyvar2 := Unbound(tyvar_id2, tyvar_level)

    | TyVar {contents = Link ty} ->
        recurse ty
    | TyCon (_, param_tys) ->
        List.iter recurse param_tys
    | TyFun (param_tys, ret_ty) ->
        List.iter recurse param_tys;
        recurse ret_ty


let rec fresh_counter = ref 0
and fresh_tyvar level _ =
  fresh_counter := !fresh_counter + 1;
  TyVar {contents = Unbound ("T" ^ string_of_int !fresh_counter, level)}

(* common types *)
let int_ty = TyCon ("Int", [])
let list_ty elem_ty = TyCon ("List", [elem_ty])
let fun_ty param_tys ret_ty = TyFun (param_tys, ret_ty)

(* ---- UNIFICATION ---- *)
(* to prevent mutually recursive modules between Error & Type ... *)
let unify_error l ty1 ty2 =
  Error.error l (TypeError
     ("Cannot unify types " ^ (string_of_type ty1) ^
      " and " ^ (string_of_type ty2) ^ "."))

let rec unify loc ty1 ty2 =
  let unify = unify loc in
  if ty1 == ty2 then
    ()
  else
    match (ty1, ty2) with
    | TyCon (name1, ty_params1), TyCon (name2, ty_params2) ->
        if name1 = name2 then
          List.iter2 unify ty_params1 ty_params2
        else
          unify_error loc ty1 ty2

    | TyVar {contents = Link ty1}, ty2
    | ty1, TyVar {contents = Link ty2} ->
        unify ty1 ty2

    | TyVar ({contents = Unbound (tyvar_id, tyvar_level)} as ty_ref), ty
    | ty, TyVar ({contents = Unbound (tyvar_id, tyvar_level)} as ty_ref) ->
        (* TODO: maybe catch the MythError and raise an error specific to
         * the two types `ty1` and `ty2`? *)
        occurs loc tyvar_id tyvar_level ty;
        ty_ref := Link ty

    | TyFun (ty_params1, ty_ret1), TyFun (ty_params2, ty_ret2) ->
        (* return types should be equal, as should argument types *)
        List.iter2 unify ty_params1 ty_params2;
        unify ty_ret1 ty_ret2

    | (ty1, ty2) ->
        unify_error loc ty1 ty2

and pairwise_unify loc tys = match tys with
  | [] -> ()
  | [ty] -> ()
  | ty1 :: ty2 :: tys ->
      unify loc ty1 ty2;
      pairwise_unify loc (ty2 :: tys)

(* ---- GENERALIZATION & INSTANTIATION ---- *)
let rec generalize level ty =
  match ty with
  | TyVar {contents = Unbound(tyvar_id, tyvar_level)} when tyvar_level > level ->
			TyVar (ref (Generic tyvar_id))

	| TyCon (name, param_tys) ->
			TyCon (name, List.map (generalize level) param_tys)

	| TyFun (param_tys, ret_ty) ->
			TyFun (List.map (generalize level) param_tys, generalize level ret_ty)

	| TyVar {contents = Link ty} -> generalize level ty

	| TyVar {contents = Generic _}
  | TyVar {contents = Unbound _} ->
      ty

let rec instantiate level ty =
	let generic_to_unbound = Hashtbl.create 10 in
  let rec recurse = function
		| TyVar {contents = Link ty} -> recurse ty
		| TyVar {contents = Generic gen_id} ->
				begin try
					Hashtbl.find generic_to_unbound gen_id
				with Not_found ->
					let tyvar = fresh_tyvar level () in
					Hashtbl.add generic_to_unbound gen_id tyvar;
					tyvar
        end

		| TyVar {contents = Unbound _} -> ty

		| TyCon (name, param_tys) ->
				TyCon (name, List.map recurse param_tys)

		| TyFun (param_tys, ret_ty) ->
				TyFun (List.map recurse param_tys, recurse ret_ty)
  in
  recurse ty


(* ---- TYPE-CHECKING ---- *)
let rec typecheck ty_env ast =
  let ty = infer 1 ty_env ast in

  match ast with
    | Ast.Bind (l, id, _) ->
        (Env.bind id ty ty_env, ty)
    | _ ->
        (Env.bind "_" ty ty_env, ty)

and infer level ty_env ast = match ast with
  | Ast.Name (l, id) -> instantiate level (Env.lookup l id ty_env)

  | Ast.Num (l, i) -> int_ty

  | Ast.List (l, asts) ->
      let elem_ty = fresh_tyvar level () in
      let tys = List.map (infer level ty_env) asts in
      pairwise_unify l tys;

      (* unify elem_ty with the first element of the list (if there is one) *)
      if tys != [] then
        unify l elem_ty (List.hd tys);

      list_ty elem_ty

  | Ast.Lambda (l, param_names, ast) ->
      let param_tys = List.map (fresh_tyvar level) param_names in
      let ty_env' = Env.bind_many param_names param_tys ty_env in
      let ret_ty = infer level ty_env' ast in
      fun_ty param_tys ret_ty

  | Ast.Bind (l, id, ast) ->
      generalize level (infer (level + 1) ty_env ast)

  | Ast.Call (l, ast, param_asts) ->
      let t1 = infer level ty_env ast in
      (* TODO: for mutation, do not instantiate variables when recursing down
       * into `infer` on the mutable arguments of the call *)
      let param_tys = List.map (infer level ty_env) param_asts in
      let ret_ty = fresh_tyvar level () in

      (* equate type of function with (tys -> ret_ty) *)
      unify l t1 (fun_ty param_tys ret_ty);
      ret_ty

  | _ -> failwith "Type.infer called on non-implemented Ast"

