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
  | TyVar {contents = Unbound (id, level)} -> "~" ^ id ^ "." ^ (string_of_int level)
  | TyVar {contents = Link ty} -> "&" ^ string_of_type ty
  | TyCon (id, []) -> id
  | TyCon (id, param_tys) ->
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
let gen_var_ty = TyVar {contents = Generic "A"}
let none_ty = TyCon ("None", [])
let bool_ty = TyCon ("Bool", [])
let int_ty = TyCon ("Int", [])
let list_gen_ty = TyCon ("List", [gen_var_ty])
let list_ty elem_ty = TyCon ("List", [elem_ty])
let fun_ty param_tys ret_ty = TyFun (param_tys, ret_ty)


(* ---- UNIFICATION ---- *)
(* to prevent mutually recursive modules between Error & Type ... *)
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
          Error.unify_error loc (string_of_type ty1) (string_of_type ty2)

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
        Error.unify_error loc (string_of_type ty1) (string_of_type ty2)

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

		| TyVar {contents = Unbound _} as ty -> ty

		| TyCon (name, param_tys) ->
				TyCon (name, List.map recurse param_tys)

		| TyFun (param_tys, ret_ty) ->
				TyFun (List.map recurse param_tys, recurse ret_ty)
  in
  recurse ty


(* ---- TYPE-CHECKING ---- *)
let rec typecheck ?level:(level=1) ty_env ast =
  let ty = infer level ty_env ast in

  match ast with
    | Ast.Bind (l, id, _)
    | Ast.Def (l, id, _, _) ->
        (Env.bind id ty ty_env, ty)
    | _ ->
        (ty_env, ty)

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

  | Ast.If (l, ast, suite1, suite2) ->
      let test_ty = infer level ty_env ast in
      let ret_ty1 = typecheck_suite level ty_env suite1 in
      let ret_ty2 = typecheck_suite level ty_env suite2 in

      unify l test_ty bool_ty;
      unify l ret_ty1 ret_ty2;

      ret_ty1

  | Ast.Call (l, ast, param_asts) ->
      let t1 = infer level ty_env ast in
      (* TODO: for mutation, do not instantiate variables when recursing down
       * into `infer` on the mutable arguments of the call *)
      let param_tys = List.map (infer level ty_env) param_asts in
      let ret_ty = fresh_tyvar level () in

      (* equate type of function with (tys -> ret_ty) *)
      unify l t1 (fun_ty param_tys ret_ty);
      ret_ty

  | Ast.Bind (l, id, ast) ->
      generalize level (infer (level + 1) ty_env ast)

  (* TODO : i have literally no idea how to do this, i have no idea if
    * this is correct at all... *)
  | Ast.Def (l, name, params, suite) ->
      let functype = fresh_tyvar (level + 1) () in
      let param_tys = List.map (fresh_tyvar (level + 1)) params in
      let ty_env' =
        Env.bind_many (name :: params) (functype :: param_tys) ty_env in

      let ret_ty = typecheck_suite (level + 1) ty_env' suite in

      unify l functype (fun_ty param_tys ret_ty);

      generalize level functype

  | _ -> failwith "Type.infer called on non-implemented Ast"

(* TODO : potentially return the last ty_env if we want the modified scope *)
and typecheck_suite level ty_env suite =
  let current_level = ref level in
  let rec recurse ty_env suite =
    match suite with
    | [] -> failwith "Type.typecheck_suite on empty suite"
    | [ast] ->
        let (_, ty) = typecheck ~level:!current_level ty_env ast in
        ty

    | (Ast.Bind _ as ast) :: asts
    | (Ast.Def _ as ast) :: asts ->
        let (ty_env, _) = typecheck ~level:!current_level ty_env ast in
        incr current_level;
        recurse ty_env asts

    | ast :: asts ->
        let (_, _) = typecheck ~level:!current_level ty_env ast in
        recurse ty_env asts

  in recurse ty_env suite

