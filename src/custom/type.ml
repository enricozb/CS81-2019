(* --------------------------------- TYPES --------------------------------- *)
type id = string
type level = int

type ty =
  | TyVar of tyvar ref
  | TyCon of id * (ty list)
  | TyFun of (ty list) * ty
  | TyRecord of tyrow
	| TyRowEmpty
	| TyRowExtend of ty Ast.NameMap.t * tyrow
  | TyFold of ((id * (ty list)) option) * (ty Lazy.t) (* for recursive types, namely classes *)
  | TyUnfold of ty

and tyvar =
  | Link of ty
  | Unbound of id * level
  | Generic of id

and tyrow = ty  (* kind of rows should only be TyRowEmpty or TyRowExtend *)

type envs = {
  ty_env : ty Env.env;
  mut_env : bool Env.env;
  val_env : Value.env_value Env.env;
}

let rec real_ty = function
  | TyVar {contents = Link ty} -> real_ty ty
  | ty -> ty

(* ------------------------------ ROW/RECORDS ------------------------------ *)
let merge_name_ty_maps name_map1 name_map2 =
	Ast.NameMap.merge
		(fun name maybe_ty_list1 maybe_ty_list2 ->
			match maybe_ty_list1, maybe_ty_list2 with
				| None, None -> assert false
				| None, (Some ty) -> Some ty
				| (Some ty), None -> Some ty
				| (Some _), (Some _) ->
            failwith "Type.merge_name_ty_maps: duplicate bindings")
		name_map1 name_map2

(* returns (ty Ast.NameMap.t * row) where row = TyRowEmpty or a tyvar *)
let rec collapse_tyrow = function
	| TyRowExtend (name_ty_map, rest_ty) ->
      begin match collapse_tyrow rest_ty with
      | (name_ty_map2, rest_ty) when Ast.NameMap.is_empty name_ty_map2 ->
          (name_ty_map, rest_ty)
      | (name_ty_map2, rest_ty) ->
          (merge_name_ty_maps name_ty_map name_ty_map2, rest_ty)
      end

	| TyVar {contents = Link ty} ->
      collapse_tyrow ty

	| TyVar _ as var ->
      (Ast.NameMap.empty, var)

	| TyRowEmpty -> (Ast.NameMap.empty, TyRowEmpty)

	| ty ->
      failwith "Type.collapse_tyrow: not a row"


(* canonicalizes at least the general types *)
let rec string_of_type ty =
  let id_to_char = Hashtbl.create 26 in
  let curr_char = ref 0 in
  let next_char () =
    let i = !curr_char in
    incr curr_char;
    let name = String.make 1 (Char.chr (97 + i mod 26)) ^
      if i >= 26 then
        string_of_int (i / 26)
      else
        ""
    in name
  in

  (* to check whether or not to wrap arrow types in () *)
  let rec recurse_tys tys =
    String.concat ", " (List.map (recurse ~toplevel:false) tys)

  and recurse ?(toplevel=false) ty = match ty with
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
        id ^ "[" ^ (recurse_tys param_tys) ^ "]"

    | TyFun (param_tys, ret_ty) ->
        (* to force evaluation of params first *)
        let single_param = List.length param_tys = 1 in
        let param_str =
          if single_param then
            recurse_tys param_tys
          else
            "(" ^ recurse_tys param_tys ^ ")"
        in
        if toplevel then
          param_str ^ " -> " ^ (recurse ret_ty)
        else
          "(" ^ param_str ^ " -> " ^ (recurse ret_ty) ^ ")"

    | TyRecord tyrow -> "{" ^ (recurse tyrow) ^ "}"

    | TyRowEmpty -> ""

    | TyRowExtend _ as tyrow ->
        let (name_ty_map, rest_ty) = collapse_tyrow tyrow in
        let name_ty_map_str =
					String.concat ", " @@
						List.map
							(fun (field, ty) -> field ^ ": " ^ (recurse ty))
							(Ast.NameMap.bindings name_ty_map)
				in
				let rest_ty_str = match real_ty rest_ty with
					| TyRowEmpty -> ""
					| TyRowExtend _ -> assert false
					| ty -> " | " ^ recurse ty
				in
				name_ty_map_str ^ rest_ty_str

    | TyFold (Some (id, []), rec_ty) ->
          id
    | TyFold (Some (id, param_tys), rec_ty) ->
          id ^ "[" ^ (recurse_tys param_tys) ^ "]"

    | TyFold (None, ty) -> recurse (Lazy.force ty)

    | TyUnfold (TyFold (_, ty)) -> recurse (Lazy.force ty)
    | TyUnfold (_) -> "Unfold(" ^ recurse ty ^ ")"

  in
  recurse ~toplevel:true ty


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

    | TyRecord tyrow ->
        recurse tyrow

		| TyRowEmpty -> ()

		| TyRowExtend (name_ty_map, rest_ty) ->
        Ast.NameMap.iter (fun field ty -> recurse ty) name_ty_map;
				recurse rest_ty

    | TyFold (Some (id, param_tys), ty) ->
        List.iter recurse param_tys
    | TyFold (None, ty) ->
        recurse (Lazy.force ty)

    | TyUnfold _ ->
        failwith "Type.occurs on TyUnfold"

let rec fresh_counter = ref 0
and fresh_tyvar level _ =
  incr fresh_counter;
  TyVar {contents = Unbound ("t" ^ string_of_int !fresh_counter, level)}

and fresh_gen_tyvar _ =
  incr fresh_counter;
  TyVar {contents = Generic ("t" ^ string_of_int !fresh_counter)}

(* ----------------------------- COMMON TYPES ----------------------------- *)
(* initialized in basis.ml *)
let int_ty = ref (TyCon ("FakeInt", []))
let string_ty = ref (TyCon ("FakeString", []))
let list_ty = ref (TyCon ("FakeList", []))
let list_of_ty = ref (fun _ -> TyCon ("FakeList", []))


let gen_var_ty = TyVar {contents = Generic "a"}
let gen_var_ty2 = TyVar {contents = Generic "b"}

let prim_int_ty = TyCon ("int", [])
let prim_string_ty = TyCon ("string", [])
let prim_list_gen_ty = TyCon ("list", [gen_var_ty])
let prim_list_ty ty = TyCon ("list", [ty])

let none_ty = TyCon ("None", [])
let bool_ty = TyCon ("Bool", [])
let prim_fun_ty param_tys ret_ty = TyFun (param_tys, ret_ty)

let rec base_record_fields () =
    List.fold_left
      (fun name_ty_map (name, ty) ->
        Ast.NameMap.add name ty name_ty_map)
    Ast.NameMap.empty [
      ("__repr__", fun_ty [] !string_ty);
    ]

and bare_record_ty name_ty_list =
  TyRecord (
    TyRowExtend (
      List.fold_left
        (fun name_ty_map (name, ty) ->
          Ast.NameMap.add name ty name_ty_map)
      (base_record_fields ()) name_ty_list,
      TyRowEmpty
    )
  )

and folded_record_ty tycon name_ty_list =
  TyFold (tycon, lazy (TyRecord (
    TyRowExtend (
      List.fold_left
        (fun name_ty_map (name, ty) ->
          Ast.NameMap.add name ty name_ty_map)
      (base_record_fields ()) name_ty_list,
      TyRowEmpty
    )
  )))

and fun_ty param_tys ret_ty =
  let prim_fun_ty = prim_fun_ty param_tys ret_ty in
  let rec inner_record = lazy (bare_record_ty [
    ("__call__", func_type);
    ("~~call~~", prim_fun_ty)
  ])
  and func_type =
    TyFold (Some ("Function", param_tys @ [ret_ty]), inner_record)
  in
  func_type

let callable_ty ?(level=0) ?(generic=false) param_tys ret_ty =
  let fun_ty = fun_ty param_tys ret_ty in
  TyFold (Some ("Callable", param_tys @ [ret_ty]), lazy (TyRecord (
    TyRowExtend (
      Ast.NameMap.singleton "__call__" fun_ty,
      if generic then
        fresh_gen_tyvar ()
      else
        fresh_tyvar level ()
    )
  )))

let has_field_ty ?(level=0) ?(generic=false) field ty =
  TyFold (None, lazy (TyRecord (
    TyRowExtend (
      Ast.NameMap.singleton field ty,
      if generic then
        fresh_gen_tyvar ()
      else
        fresh_tyvar level ()
    )
  )))

(* ------------------------------ UNIFICATION ------------------------------ *)
let rec unify loc ty1 ty2 =
  let unify = unify loc in
  if ty1 == ty2 then
    ()
  else
    match (ty1, ty2) with
    | TyCon (name1, ty_params1), TyCon (name2, ty_params2) ->
        if name1 = name2 && List.length ty_params1 = List.length ty_params2 then
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
        (* TODO : improve this error... it's often just a simple
         * call error, where the incorrect number of arguments were
         * provided *)
        if List.length ty_params1 <> List.length ty_params2 then
          Error.unify_error loc (string_of_type ty1) (string_of_type ty2);

        (* return types should be equal, as should argument types *)
        List.iter2 unify ty_params1 ty_params2;
        unify ty_ret1 ty_ret2

    | TyRecord tyrow1, TyRecord tyrow2 ->
        unify tyrow1 tyrow2

		| TyRowEmpty, TyRowEmpty -> ()

    | (TyRowExtend _ as tyrow1), (TyRowExtend _ as tyrow2) ->
        unify_rows loc tyrow1 tyrow2

    | TyRowEmpty, TyRowExtend (name_ty_map, _)
    | TyRowExtend (name_ty_map, _), TyRowEmpty ->
				let field, _ = Ast.NameMap.choose name_ty_map in
				Error.missing_field loc field

    | TyUnfold (TyFold (_, ty1)), ty2
    | ty2, TyUnfold (TyFold (_, ty1)) ->
        unify (Lazy.force ty1) ty2

    (* TODO: is this the right way to check the names? *)
    | TyFold (Some (id1, param_tys1), rec_ty1),
      TyFold (Some (id2, param_tys2), rec_ty2) ->
          if id1 = id2 then
            unify (TyCon (id1, param_tys1)) (TyCon (id2, param_tys2))
          else begin try
            unify (Lazy.force rec_ty1) (Lazy.force rec_ty2)
          with Error.MythError _ ->
            Error.unify_error loc (string_of_type ty1) (string_of_type ty2)
          end

    | TyFold (_, ty1), TyFold (_, ty2) ->
        unify (Lazy.force ty1) (Lazy.force ty2)

    (* TODO: no idea if this should be here... *)
    | TyFold (Some (name1, params1), _), TyCon (name2, params2)
    | TyCon (name1, params1), TyFold (Some (name2, params2), _) ->
        unify (TyCon (name1, params1)) (TyCon (name2, params2))

    | (ty1, ty2) ->
        Error.unify_error loc (string_of_type ty1) (string_of_type ty2)


and unify_rows loc tyrow1 tyrow2 =
  let name_ty_map1, rest_ty1 = collapse_tyrow tyrow1 in
	let name_ty_map2, rest_ty2 = collapse_tyrow tyrow2 in

  let bind_distinct_names name_ty_map name_ty_list =
    List.fold_left
      (fun name_ty_map (name, ty) ->
        assert (not (Ast.NameMap.mem name name_ty_map));
        Ast.NameMap.add name ty name_ty_map)
    name_ty_map name_ty_list
  in

  let rec unify_names missing1 missing2 names1 names2 =
    match (names1, names2) with
    | [], [] -> missing1, missing2
    | ([], _) -> (bind_distinct_names missing1 names2, missing2)
		| (_, []) -> (missing1, bind_distinct_names missing2 names1)
    | ((name1, ty1) :: rest1, (name2, ty2) :: rest2) ->
      begin match Ast.compare_names name1 name2 with
      | 0 ->
          unify loc ty1 ty2;
          unify_names missing1 missing2 rest1 rest2

      | x when x < 0 ->
          unify_names missing1 (Ast.NameMap.add name1 ty1 missing2) rest1 names2

      | x ->
          unify_names (Ast.NameMap.add name2 ty2 missing1) missing2 names1 rest2
      end
  in

  let (missing1, missing2) =
      unify_names Ast.NameMap.empty Ast.NameMap.empty
        (Ast.NameMap.bindings name_ty_map1)
        (Ast.NameMap.bindings name_ty_map2)
  in

  match (Ast.NameMap.is_empty missing1, Ast.NameMap.is_empty missing2) with
		| (true, true) -> unify loc rest_ty1 rest_ty2
		| (true, false) -> unify loc (TyRowExtend (missing2, rest_ty1)) rest_ty2
		| (false, true) -> unify loc rest_ty1 (TyRowExtend (missing1, rest_ty2))
		| (false, false) ->
				begin match rest_ty1 with
					| TyRowEmpty ->
							(* will result in an error *)
							unify loc rest_ty1 (TyRowExtend (missing1, fresh_tyvar 0 ()))

					| TyVar ({contents = Unbound (_, level)} as ty_ref) ->
							let new_rest_row_var = fresh_tyvar level () in
							unify loc rest_ty2 (TyRowExtend (missing2, new_rest_row_var));
							begin match !ty_ref with
								| Link _ -> Error.type_error loc "recursive types"
								| _ -> ()
              end;
							unify loc rest_ty1 (TyRowExtend (missing1, new_rest_row_var))

					| _ -> assert false
        end

and pairwise_unify loc tys = match tys with
  | [] -> ()
  | [ty] -> ()
  | ty1 :: ty2 :: tys ->
      unify loc ty1 ty2;
      pairwise_unify loc (ty2 :: tys)


(* -------------------- GENERALIZATION & INSTANTIATION -------------------- *)
let rec generalize level ty =
  match ty with
  | TyVar {contents = Unbound(tyvar_id, tyvar_level)} when tyvar_level > level ->
			TyVar (ref (Generic tyvar_id))
	| TyVar {contents = Link ty} -> generalize level ty

	| TyCon (name, param_tys) ->
			TyCon (name, List.map (generalize level) param_tys)

	| TyFun (param_tys, ret_ty) ->
			TyFun (List.map (generalize level) param_tys, generalize level ret_ty)

  | TyRecord tyrow -> TyRecord (generalize level tyrow)

  | TyRowExtend (name_ty_map, rest_ty) ->
        TyRowExtend (Ast.NameMap.map (generalize level) name_ty_map,
                     generalize level rest_ty)

	| TyVar {contents = Generic _}
  | TyVar {contents = Unbound _}

  | TyRowEmpty as ty -> ty

  | TyFold (None, ty) ->
      TyFold (None, lazy (generalize level (Lazy.force ty)))
  | TyFold (Some (id, param_tys), ty) ->
      TyFold (Some (id, List.map (generalize level) param_tys),
              lazy (generalize level @@ Lazy.force ty))

  | TyUnfold _ ->
      failwith "Type.generalize on TyUnfold"

let rec instantiate level ty =
	let generic_to_unbound = Hashtbl.create 100 in
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

    | TyRecord tyrow -> TyRecord (recurse tyrow)

		| TyRowEmpty as ty -> ty

		| TyRowExtend (name_ty_map, rest_ty) ->
				TyRowExtend (Ast.NameMap.map recurse name_ty_map, recurse rest_ty)

  | TyFold (None, ty) ->
      TyFold (None, lazy (recurse (Lazy.force ty)))
  | TyFold (Some (id, param_tys), ty) ->
      TyFold (Some (id, List.map recurse param_tys),
              lazy (recurse (Lazy.force ty)))


  | TyUnfold _ ->
      failwith "Type.instantiate on TyUnfold"
  in
  recurse ty


(* ---------------------------- SUITE FUNCTIONS ---------------------------- *)

(* returns true if all branches of this suite return *)
let rec suite_always_returns suite = match suite with
  | [] -> false
  | Ast.Return (_, ast) :: [] -> true
  | Ast.Return (l, _) :: rest ->
      Error.unreachable_code_error l "return"

  | Ast.If (l, _, suite1, suite2) :: rest ->
      if suite_always_returns suite1 && suite_always_returns suite2 then
        if rest = [] then
          true
        else
          Error.unreachable_code_error l "return"
      else
        suite_always_returns rest

  | _ :: rest -> suite_always_returns rest


(* --------------------------- VALUE RESTRICTION --------------------------- *)
let is_expansive = function
  (* TODO : when mutable values exist, List needs to make sure it
   * doesn't contain any mutable values *)
  | Ast.Name _
  | Ast.Num _
  | Ast.String _
  | Ast.List _
  | Ast.Lambda _
  | Ast.Field _  (* TODO: change if getters & setters are added *)
    -> false

  | Ast.Record _ (* TODO: change this with mutable records *)
  | Ast.Call _ -> true
  | _ -> failwith "Type.is_expansive called on non-expression"


(* ----------------------------- TYPE-CHECKING ----------------------------- *)
(* properties of suites returned on typecheck_suite *)
type suite_props =
    { ret_tys    : ty list;
      flow_stmts : bool;
    }

let rec typecheck ?level:(level=1) envs ast =
  let ty = infer level envs ast in

  match ast with
    | Ast.Bind (l, mut, id, _) ->
        ({envs with
          ty_env = Env.bind id ty envs.ty_env;
          mut_env = Env.bind id mut envs.mut_env;
        }, ty)

    | Ast.Def (l, id, _, _) ->
        ({envs with
          ty_env = Env.bind id ty envs.ty_env;
          mut_env = Env.bind id false envs.mut_env;
        }, ty)

    | _ ->
        (envs, ty)

and infer level envs ast = match ast with
  | Ast.Name (l, id) -> instantiate level (Env.lookup l id envs.ty_env)

  | Ast.Num (l, i) -> prim_int_ty

  | Ast.String (l, s) -> prim_string_ty

  | Ast.List (l, asts) ->
      let elem_ty = fresh_tyvar level () in
      let tys = List.map (infer level envs) asts in
      pairwise_unify l tys;

      (* unify elem_ty with the first element of the list (if there is one) *)
      if tys != [] then
        unify l elem_ty (List.hd tys);

      prim_list_ty elem_ty

  | Ast.Record (l, name_ast_map) ->
      let name_ty_map =
				Ast.NameMap.map
          (infer level envs)
					name_ast_map
			in
      let name_ty_map =
        Ast.NameMap.union
          (fun _ ty1 ty2 -> Some ty2)
          (base_record_fields ())
          name_ty_map
      in
      if Ast.NameMap.is_empty name_ty_map then
        TyFold (None, lazy (TyRecord TyRowEmpty))
      else
        TyFold (None, lazy (TyRecord (TyRowExtend (name_ty_map, TyRowEmpty))))

  | Ast.Field (l, ast, name) ->
      let rest_ty = fresh_tyvar level () in
			let field_ty = fresh_tyvar level () in
			let record_ty = TyFold (None, lazy (TyRecord
        (TyRowExtend (Ast.NameMap.singleton name field_ty, rest_ty))))
      in
			unify l record_ty (infer level envs ast);
			field_ty

  | Ast.Lambda (l, param_names, ast) ->
      let param_tys = List.map (fresh_tyvar level) param_names in
      let ty_env' = Env.bind_many param_names param_tys envs.ty_env in
      let ret_ty = infer level {envs with ty_env = ty_env'} ast in
      fun_ty param_tys ret_ty

  | Ast.If (l, ast, suite1, suite2) ->
      let test_ty = infer level envs ast in
      let suite_props1 = typecheck_suite level envs suite1 in
      let suite_props2 = typecheck_suite level envs suite2 in

      if (suite_props1.ret_tys, suite_props2.ret_tys) <> ([], []) then
        Error.return_outside_def l;

      if suite_props1.flow_stmts || suite_props2.flow_stmts then
        Error.flow_outside_loop l;

      unify l test_ty bool_ty;
      none_ty

  | Ast.While (l, ast, suite) ->
      let test_ty = infer level envs ast in
      let suite_props = typecheck_suite level envs suite in
      if suite_props.ret_tys <> [] then
        Error.return_outside_def l;

      unify l test_ty bool_ty;
      none_ty

  | Ast.Call (l, ast, param_asts) ->
      let t1 = infer level envs ast in
      let param_tys = List.map (infer level envs ) param_asts in
      let ret_ty = fresh_tyvar level () in

      (* equate type of function with (param_tys -> ret_ty) *)
      unify l t1 (callable_ty ~level:level param_tys ret_ty);
      ret_ty

  | Ast.Bind (l, mut, id, ast) ->
      let ty = infer (level + 1) envs ast in

      if mut || is_expansive ast then
        ty
      else
        generalize level ty

  | Ast.Assign (l, id, ast) ->
      (* TODO: really not sure about this level ... Notice it is not +1 like
       * in Ast.Bind. Because I don't really think this is a let-in type
       * expression. *)
      if not (Env.lookup l id envs.mut_env) then
        Error.bind_error l id;

      let ast_ty = infer level envs ast in
      unify l ast_ty (Env.lookup l id envs.ty_env);
      ast_ty

  (* TODO : do a thorough check of whether or not the level logic is correct *)
  | Ast.Def (l, name, params, suite) ->
      let functype = fresh_tyvar (level + 1) () in
      let param_tys = List.map (fresh_tyvar (level + 1)) params in
      let ty_env' =
        Env.bind_many (name :: params) (functype :: param_tys) envs.ty_env in

      (* none of the parameters, nor the function name is mutable
       * TODO: maybe make function parameters potentially mutable? *)
      let len = List.length (name :: params) in
      let mut_env' =
        Env.bind_many
        (name :: params)
        (List.init len (fun _ -> false)) envs.mut_env in

      let envs' = {envs with ty_env = ty_env'; mut_env = mut_env'} in

      let suite_props = typecheck_suite (level + 1) envs' suite in

      if suite_props.flow_stmts then
        Error.flow_outside_loop l;

      if suite_always_returns suite then
        pairwise_unify l suite_props.ret_tys
      else
        pairwise_unify l (none_ty :: suite_props.ret_tys);

      let ret_ty = match suite_props.ret_tys with
        | [] -> none_ty
        | ret_ty :: _ -> ret_ty
      in

      unify l functype (fun_ty param_tys ret_ty);

      generalize level functype

  | Ast.Return (l, _) ->
      Error.return_outside_def l;

  (*| Ast.Class (l, name, suite) ->*)
      (*let instance_funcs, class_funcs = Ast.class_split_suite suite in*)

  | Ast.Continue l | Ast.Break l ->
      Error.flow_outside_loop l;

  | _ -> failwith "Type.infer called on non-implemented Ast"

(* TODO : potentially return the last ty_env if we want the modified scope *)
and typecheck_suite level envs suite =
  let current_level = ref level in
  let rec recurse envs suite_props suite =
    match suite with
    | [] -> suite_props

    | Ast.Return (l, ast) :: [] ->
        let (_, ty) = typecheck ~level:!current_level envs ast in
        {suite_props with ret_tys = ty :: suite_props.ret_tys}

    | Ast.Return (l, _) :: rest ->
        Error.unreachable_code_error l "return"

    | Ast.Break l :: []
    | Ast.Continue l :: [] ->
        {suite_props with flow_stmts = true}

    | Ast.Break l :: rest ->
        Error.unreachable_code_error l "break"
    | Ast.Continue l :: rest ->
        Error.unreachable_code_error l "continue"

    | Ast.If (l, test, suite1, suite2) :: rest ->
      let test_ty = infer level envs test in
      let suite_props1 = typecheck_suite level envs suite1 in
      let suite_props2 = typecheck_suite level envs suite2 in

      unify l test_ty bool_ty;
      recurse
        envs
        {suite_props with ret_tys =
          suite_props.ret_tys @ suite_props1.ret_tys @ suite_props2.ret_tys}
        rest

    | (Ast.Bind _ as ast) :: suite
    | (Ast.Def _ as ast) :: suite ->
        let (envs, ty) =
          typecheck ~level:!current_level envs ast
        in
        incr current_level;
        recurse envs suite_props suite

    | ast :: asts ->
        let (envs, ty) = typecheck ~level:!current_level envs ast in
        recurse envs suite_props asts

  in recurse envs {ret_tys = []; flow_stmts = false} suite

