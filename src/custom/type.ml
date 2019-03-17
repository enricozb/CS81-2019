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


(* canonicalizes at least the general types *)
let rec string_of_type ty =
  let id_to_char = Hashtbl.create 26 in
  let curr_char = ref 0 in
  let next_char () =
    let i = !curr_char in
    incr curr_char;
    let name = String.make 1 (Char.chr (65 + i mod 26)) ^
      if i >= 26 then
        string_of_int (i / 26)
      else
        ""
    in name
  in

  let rec recurse_tys tys = String.concat ", " (List.map recurse tys)
  and recurse ty = match ty with
    | TyVar {contents = Generic id} ->
        begin try
          Hashtbl.find id_to_char id
        with Not_found ->
          let newid = next_char () in
          Hashtbl.add id_to_char id newid;
          newid
        end

    | TyVar {contents = Unbound (id, _)} ->
        "_" ^ id

    | TyVar {contents = Link ty} ->
        recurse ty

    | TyCon (id, []) -> id
    | TyCon (id, param_tys) ->
        id ^ "<" ^ (recurse_tys param_tys) ^ ">"

    | TyFun (param_tys, ret_ty) ->
        (* to force evaluation of params first *)
        let param_str = recurse_tys param_tys in
        "{" ^ param_str ^ "} -> " ^ (recurse ret_ty)
  in
  recurse ty


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


(* ---- SUITE FUNCTIONS ---- *)

(* returns true if all branches of this suite return *)
let rec suite_always_returns suite = match suite with
  | [] -> false
  | Ast.Return (_, ast) :: [] -> true
  | Ast.Return (l, _) :: rest ->
      Error.unreachable_code_error l

  | Ast.If (l, _, suite1, suite2) :: rest ->
      if suite_always_returns suite1 && suite_always_returns suite2 then
        if rest = [] then
          true
        else
          Error.unreachable_code_error l
      else
        suite_always_returns rest

  | _ :: rest -> suite_always_returns rest


(* ---- VALUE RESTRICTION ---- *)
let is_expansive = function
  (* TODO : when mutable values exist, List needs to make sure it
   * doesn't contain any mutable values *)
  | Ast.Name _ | Ast.Num _ | Ast.List _ | Ast.Lambda _ -> false
  | Ast.Call _ -> true
  | _ -> failwith "Type.is_expansive called on non-expression"


(* ---- TYPE-CHECKING ---- *)
let rec typecheck ?level:(level=1) ty_env mut_env ast =
  let ty = infer level ty_env mut_env ast in

  match ast with
    | Ast.Bind (l, mut, id, _) ->
        (Env.bind id ty ty_env, Env.bind id mut mut_env, ty)

    | Ast.Def (l, id, _, _) ->
        (Env.bind id ty ty_env, Env.bind id false mut_env, ty)

    | _ ->
        (ty_env, mut_env, ty)

and infer level ty_env mut_env ast = match ast with
  | Ast.Name (l, id) -> instantiate level (Env.lookup l id ty_env)

  | Ast.Num (l, i) -> int_ty

  | Ast.List (l, asts) ->
      let elem_ty = fresh_tyvar level () in
      let tys = List.map (infer level ty_env mut_env) asts in
      pairwise_unify l tys;

      (* unify elem_ty with the first element of the list (if there is one) *)
      if tys != [] then
        unify l elem_ty (List.hd tys);

      list_ty elem_ty

  | Ast.Lambda (l, param_names, ast) ->
      let param_tys = List.map (fresh_tyvar level) param_names in
      let ty_env' = Env.bind_many param_names param_tys ty_env in
      let ret_ty = infer level ty_env' mut_env ast in
      fun_ty param_tys ret_ty

  | Ast.If (l, ast, suite1, suite2) ->
      let test_ty = infer level ty_env mut_env ast in
      let ret_tys1 = typecheck_suite level ty_env mut_env suite1 in
      let ret_tys2 = typecheck_suite level ty_env mut_env suite2 in

      if (ret_tys1, ret_tys2) <> ([], []) then
        Error.return_outside_def l;

      unify l test_ty bool_ty;
      none_ty

  | Ast.Call (l, ast, param_asts) ->
      let t1 = infer level ty_env mut_env ast in
      let param_tys = List.map (infer level ty_env mut_env) param_asts in
      let ret_ty = fresh_tyvar level () in

      (* equate type of function with (tys -> ret_ty) *)
      unify l t1 (fun_ty param_tys ret_ty);
      ret_ty

  | Ast.Bind (l, mut, id, ast) ->
      let ty = infer (level + 1) ty_env mut_env ast in

      if mut || is_expansive ast then
        ty
      else
        generalize level ty

  | Ast.Assign (l, id, ast) ->
      (* TODO: really not sure about this level ... notice it is not +1 like
       * in Ast.Bind. Because I don't really think this is a let-in type
       * expression. *)
      if not (Env.lookup l id mut_env) then
        Error.bind_error l id;

      let ast_ty = infer level ty_env mut_env ast in
      unify l ast_ty (Env.lookup l id ty_env);
      ast_ty

  (* TODO : i have literally no idea how to do this, i have no idea if
    * this is correct at all... *)
  | Ast.Def (l, name, params, suite) ->
      let functype = fresh_tyvar (level + 1) () in
      let param_tys = List.map (fresh_tyvar (level + 1)) params in
      let ty_env' =
        Env.bind_many (name :: params) (functype :: param_tys) ty_env in

      (* none of the parameters, nor the function name is mutable
       * TODO: maybe make function parameters potentially mutable? *)
      let len = List.length (name :: params) in
      let mut_env' =
        Env.bind_many
        (name :: params)
        (List.init len (fun _ -> false)) mut_env in

      let ret_tys = typecheck_suite (level + 1) ty_env' mut_env' suite in

      if suite_always_returns suite then
        pairwise_unify l ret_tys
      else
        pairwise_unify l (none_ty :: ret_tys);

      let ret_ty = match ret_tys with
        | [] -> none_ty
        | ret_ty :: _ -> ret_ty
      in

      unify l functype (fun_ty param_tys ret_ty);

      generalize level functype

  | _ -> failwith "Type.infer called on non-implemented Ast"

(* TODO : potentially return the last ty_env if we want the modified scope *)
and typecheck_suite level ty_env mut_env suite =
  let current_level = ref level in
  let rec recurse ty_env mut_env ret_tys suite =
    match suite with
    | [] -> ret_tys

    | Ast.Return (l, ast) :: [] ->
        let (_, _, ty) = typecheck ~level:!current_level ty_env mut_env ast in
        ty :: ret_tys
    | Ast.Return (l, _) :: rest ->
        Error.unreachable_code_error l

    | Ast.If (l, test, suite1, suite2) :: rest ->
      let test_ty = infer level ty_env mut_env test in
      let ret_tys1 = typecheck_suite level ty_env mut_env suite1 in
      let ret_tys2 = typecheck_suite level ty_env mut_env suite2 in

      unify l test_ty bool_ty;
      recurse ty_env mut_env (ret_tys1 @ ret_tys2) rest

    | (Ast.Bind _ as ast) :: suite
    | (Ast.Def _ as ast) :: suite ->
        let (ty_env, mut_env, _) =
          typecheck ~level:!current_level ty_env mut_env ast
        in
        incr current_level;
        recurse ty_env mut_env ret_tys suite

    | ast :: asts ->
        let (_, _, _) = typecheck ~level:!current_level ty_env mut_env ast in
        recurse ty_env mut_env ret_tys asts

  in recurse ty_env mut_env [] suite

