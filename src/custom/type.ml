module StringSet = BatSet.String

(* --------------------------------- TYPES --------------------------------- *)
type id = string
type level = int

type ty =
  | TyVar of tyvar ref
  | TyCon of id * (ty list)
  | TyFun of (ty list) * ty
  | TyRecord of ty Ast.NameMap.t

  (* TyFold is for recursive types. The (id * (ty list)) portion is meant to *)
  (* be identical to TyCon. TyFold, if the first part of the tuple is not
   * None, is a TyCon with a TyRecord backing it. If the ty-con portion of
   * TyFold is None, then it's just a record? This field should not be
   * optional. *)
  | TyFold of ((id * (ty list)) option) * (ty Lazy.t)
  | TyUnfold of ty

and tyvar =
  | Link of ty
  | Unbound of id * level * (traits option)
  | Generic of id * (traits option)

and traits = (ty BatDynArray.t)

type kind =
  | KindFun of kind_fun
  | KindVar of ty

and kind_fun = (ty list) -> ty

let kind_fun_0 ty =
  let kind_fun param_tys =
    if param_tys <> [] then
      failwith "Incorrect CON length"
    else
      ty
  in
  kind_fun

let kind_fun_1 ty_fun =
  let kind_fun param_tys =
    if List.length param_tys <> 1 then
      failwith "Incorrect CON length"
    else
      ty_fun (List.nth param_tys 0)
  in
  kind_fun

type envs = {
  ty_env : ty Env.env;
  kind_env : kind Env.env;
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

(* --------------------------- CANONICALIZATION --------------------------- *)
(* canonicalizes at least the generic types & their trait bounds *)
let rec string_of_type ty =
  let id_to_char = BatHashtbl.create 26 in
  let id_to_traits = BatHashtbl.create 26 in
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
    | TyVar {contents = Generic (id, traits)} ->
        begin try
          BatHashtbl.find id_to_char id
        with Not_found ->
          let newid = next_char () in

          (* compute the string of the type dependent on whether or not
           * it has traits. If it has traits, it will be a string like
           *
           *   <a: Showable + Iterable[b]>
           *
           * if it doesn't, it'll just be something like `a`
           *
           * traits are memoized and only used right before return, so
           * types look something like
           *
           *   <a: Showable> => Function[a, String]
           *)
          begin match traits with
          | Some traits ->
            let traits = List.map recurse (BatDynArray.to_list traits) in
            let traits_str = String.concat " + " traits in
            BatHashtbl.add id_to_traits newid traits_str
          | None -> ()
          end;

          BatHashtbl.add id_to_char id newid;
          newid
        end

    | TyVar {contents = Unbound (id, _, None)} ->
        "_" ^ id
    | TyVar {contents = Unbound (id, _, Some traits)} ->
        begin try
          BatHashtbl.find id_to_char id
        with Not_found ->
          let traits = List.map recurse (BatDynArray.to_list traits) in
          let traits_str = String.concat " + " traits in
          BatHashtbl.add id_to_traits id traits_str;
          BatHashtbl.add id_to_char id ("_" ^ id);
          "_" ^ id
        end

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

    | TyRecord name_ty_map ->
        let name_ty_map_str =
					String.concat ", " @@
						List.map
							(fun (field, ty) -> field ^ ": " ^ (recurse ty))
							(Ast.NameMap.bindings name_ty_map)
				in
        "{" ^ name_ty_map_str ^ "}"

    | TyFold (Some (id, []), _) ->
        id
    | TyFold (Some (id, param_tys), _) ->
        id ^ "[" ^ (recurse_tys param_tys) ^ "]"

    | TyFold (None, ty) -> recurse (Lazy.force ty)

    | TyUnfold (TyFold (_, ty)) -> recurse (Lazy.force ty)
    | TyUnfold (_) -> "Unfold(" ^ recurse ty ^ ")"

  in
  let ty_str = recurse ~toplevel:true ty in
  let trait_bindings = BatHashtbl.bindings id_to_traits in
  if trait_bindings = [] then
    ty_str
  else
    let trait_strings =
      List.map (fun (id, traits) -> id ^ ": " ^ traits) trait_bindings
    in
    let traits = "<" ^ (String.concat ", " trait_strings) ^ ">" in
    traits ^ " => " ^ ty_str


(* errors if occurs check fails, otherwise returns unit *)
let rec occurs loc tyvar_id tyvar_level ty =
  let recurse = occurs loc tyvar_id tyvar_level in

  match ty with
    | TyVar {contents = Generic _} -> assert false
    | TyVar ({contents = Unbound (tyvar_id2, tyvar_level2, traits)} as tyvar2) ->
      (* check if we occur in any of the traits *)
      begin match traits with
      | Some traits ->
          BatDynArray.iter recurse traits
      | None -> ()
      end;

      (* if this doesn't error out, we can update the level of tyvar2 for
       * reasons I still don't understand... *)
      if tyvar_id = tyvar_id2 then
        Error.type_error loc "recursive types"
      else if tyvar_level2 > tyvar_level then
        tyvar2 := Unbound (tyvar_id2, tyvar_level, traits)

    | TyVar {contents = Link ty} ->
        recurse ty

    | TyCon (_, param_tys) ->
        List.iter recurse param_tys

    | TyFun (param_tys, ret_ty) ->
        List.iter recurse param_tys;
        recurse ret_ty

		| TyRecord (name_ty_map) ->
        Ast.NameMap.iter (fun field ty -> recurse ty) name_ty_map

    | TyFold (Some (id, param_tys), ty) ->
        List.iter recurse param_tys
    | TyFold (None, ty) ->
        recurse (Lazy.force ty)

    | TyUnfold _ ->
        failwith "Type.occurs on TyUnfold"

let rec fresh_counter = ref 0
and fresh_tyvar level ?(traits=None) _ =
  incr fresh_counter;
  TyVar {contents = Unbound ("t" ^ string_of_int !fresh_counter, level, traits)}

and fresh_gen_tyvar _ =
  incr fresh_counter;
  TyVar {contents = Generic ("t" ^ string_of_int !fresh_counter, None)}

(* ----------------------------- COMMON TYPES ----------------------------- *)
(* initialized in basis.ml *)
let int_ty = ref (TyCon ("FakeInt", []))
let string_ty = ref (TyCon ("FakeString", []))
let list_ty = ref (TyCon ("FakeList", []))
let list_of_ty = ref (fun _ -> TyCon ("FakeList", []))


let gen_var_ty = TyVar {contents = Generic ("a", None)}
let gen_var_ty2 = TyVar {contents = Generic ("b", None)}

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
    List.fold_left
      (fun name_ty_map (name, ty) -> Ast.NameMap.add name ty name_ty_map)
      (base_record_fields ())
      name_ty_list
  )

and folded_record_ty tycon name_ty_list =
  TyFold (tycon, lazy (TyRecord (
      List.fold_left
        (fun name_ty_map (name, ty) -> Ast.NameMap.add name ty name_ty_map)
        (base_record_fields ())
        name_ty_list
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

let callable_trait param_tys ret_ty =
  let fun_ty = fun_ty param_tys ret_ty in
  TyFold (Some ("Callable", param_tys @ [ret_ty]), lazy (TyRecord (
      Ast.NameMap.singleton "__call__" fun_ty
  )))

let has_field_trait ?(tycon=None) field ty =
  TyFold (tycon, lazy (TyRecord (
      Ast.NameMap.singleton field ty
  )))

let callable_ty ?(level=0) param_tys ret_ty =
  let traits = BatDynArray.of_list [callable_trait param_tys ret_ty] in
  fresh_tyvar level ~traits:(Some traits) ()

let has_field_ty ?(level=0) ?(tycon=None) field ty =
  let traits = BatDynArray.of_list [has_field_trait ~tycon:tycon field ty] in
  fresh_tyvar level ~traits:(Some traits) ()

(* -------------------------------- TRAITS -------------------------------- *)
let rec check_record_conflict loc record1 record2 =
  let (fields1, _) = List.split (Ast.NameMap.bindings record1) in
  let (fields2, _) = List.split (Ast.NameMap.bindings record2) in

  (* these are fields that the trait has but the conforming type doesn't *)
  let shared_fields =
    StringSet.inter (StringSet.of_list fields1) (StringSet.of_list fields2)
  in

  (* unify types in the trait *)
  StringSet.iter
    (fun field ->
      let ty1 = Ast.NameMap.find field record1 in
      let ty2 = Ast.NameMap.find field record2 in
      check_trait_conflict loc ty1 ty2
    )
    shared_fields

(* check that these two traits don't conflict *)
and check_trait_conflict loc trait1 trait2 =
  match trait1, trait2 with
  | TyFold (_, record1), TyFold (_, record2) ->
      (* TODO: halt on already seen id1 to id2, or else we'll infinite loop *)
      check_trait_conflict loc (Lazy.force record1) (Lazy.force record2)

  | TyFold (_, record1), TyRecord _ ->
      check_trait_conflict loc (Lazy.force record1) trait2

  | TyRecord _, TyFold(_, record2) ->
      check_trait_conflict loc trait1 (Lazy.force record2)

  | TyRecord record1, TyRecord record2 ->
      check_record_conflict loc record1 record2

  | _ ->
      unify loc trait1 trait2

(* check that no traits in `traits2` conflict with those in `traits1` *)
and check_traits_conflict loc traits1 traits2 =
  BatDynArray.iter
    (fun trait2 ->
      BatDynArray.iter
        (fun trait1 -> check_trait_conflict loc trait1 trait2)
        traits1
    )
    traits2;

  (* we know no traits conflict, but we want to return the traits in traits2
   * that aren't already basically present in traits1 *)
  BatDynArray.filter
    (fun t2 ->
      let rec iter rest = match rest with
        | [] -> true
        | t1 :: rest ->
          try
            conforms_to_trait loc t2 t1;
            false
          (* TODO: ooooo catching this giant error seems weird af *)
          with Error.MythError (_, _) ->
            iter rest
      in
      iter (BatDynArray.to_list traits1)
    )
    traits2

(* checks if `record2` conforms to `record1`. Basically if all of the fields in
 * `record1` are also in `record2`, and the types match. *)
and conforms_to_record loc record1 record2 =
  let (fields1, _) = List.split (Ast.NameMap.bindings record1) in
  let (fields2, _) = List.split (Ast.NameMap.bindings record2) in

  (* these are fields that the trait has but the conforming type doesn't *)
  let missing_fields =
    StringSet.diff (StringSet.of_list fields1) (StringSet.of_list fields2)
  in

  if StringSet.cardinal missing_fields > 0 then begin
    (*Printf.printf "Missing %i fields.\n" (StringSet.cardinal missing_fields);*)
    Error.missing_field loc (StringSet.any missing_fields)
  end;

  (* unify types in the trait *)
  Ast.NameMap.iter
    (fun field ty1 ->
      let ty2 = Ast.NameMap.find field record2 in
      conforms_to_trait loc ty1 ty2
    )
    record1

and conforms_to_trait loc trait ty =
  match (trait, ty) with
  | TyFold (tycon1, record1), TyFold (tycon2, record2) ->
      begin match tycon1, tycon2 with
        | Some (id1, ty_params1), Some (id2, ty_params2) ->
            unify loc (TyCon (id1, ty_params1)) (TyCon (id2, ty_params2))
        | _ ->
          conforms_to_trait loc (Lazy.force record1) (Lazy.force record2)
      end

  | TyFold (_, record1), TyRecord _ ->
      conforms_to_trait loc (Lazy.force record1) ty

  | TyRecord _, TyFold(_, record2) ->
      conforms_to_trait loc trait (Lazy.force record2)

  | TyRecord record1, TyRecord record2 ->
      conforms_to_record loc record1 record2

  | _ ->
      unify loc trait ty

and conforms_to_traits loc traits ty =
  BatDynArray.iter (fun trait -> conforms_to_trait loc trait ty) traits

(* ------------------------------ UNIFICATION ------------------------------ *)
and unify loc ty1 ty2 =
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

    | TyVar ({contents = Unbound (tyvar_id, tyvar_level, None)} as ty_ref), ty
    | ty, TyVar ({contents = Unbound (tyvar_id, tyvar_level, None)} as ty_ref) ->
        (* TODO: maybe catch the MythError and raise an error specific to
         * the two types `ty1` and `ty2`? *)
        occurs loc tyvar_id tyvar_level ty;
        ty_ref := Link ty

    | TyVar ({contents = Unbound (id1, level1, Some traits1)} as ty_ref1),
      TyVar ({contents = Unbound (id2, level2, Some traits2)} as ty_ref2) ->
        (* WLOG: add all of ty2's traits to ty1's, and point ty2 to ty1. *)
        (* TODO: no idea if both occurs are necessary *)
        occurs loc id1 level1 ty2;
        occurs loc id2 level2 ty1;

        (* check that all of traits2 don't conflict with any of traits1 *)
        let unique_traits = check_traits_conflict loc traits1 traits2 in
        (* add all elements of traits2 to traits1 *)
        BatDynArray.append unique_traits traits1;

        ty_ref2 := Link ty1

    | TyVar ({contents = Unbound (tyvar_id, tyvar_level, Some traits)} as ty_ref), ty
    | ty, TyVar ({contents = Unbound (tyvar_id, tyvar_level, Some traits)} as ty_ref) ->
        (* TODO: maybe catch the MythError and raise an error specific to
         * the two types `ty1` and `ty2`? *)
        occurs loc tyvar_id tyvar_level ty;
        conforms_to_traits loc traits ty;
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

    | TyRecord record1, TyRecord record2 ->
        unify_records loc record1 record2

    | TyUnfold (TyFold (_, ty1)), ty2
    | ty2, TyUnfold (TyFold (_, ty1)) ->
        unify (Lazy.force ty1) ty2

    (* TyFolds are TyCon's with objcts underlying the type names. So, we
     * check them just like type cons, if they are named. Otherwise, they
     * just describe plain objects. *)
    | TyFold (Some (id1, param_tys1), rec_ty1),
      TyFold (Some (id2, param_tys2), rec_ty2) ->
          unify (TyCon (id1, param_tys1)) (TyCon (id2, param_tys2))

    | TyFold (_, ty1), TyFold (_, ty2) ->
        unify (Lazy.force ty1) (Lazy.force ty2)

    (* TODO: no idea if this should be here... *)
    | TyFold (Some (name1, params1), _), TyCon (name2, params2)
    | TyCon (name1, params1), TyFold (Some (name2, params2), _) ->
        unify (TyCon (name1, params1)) (TyCon (name2, params2))

    | (ty1, ty2) ->
        Error.unify_error loc (string_of_type ty1) (string_of_type ty2)


and unify_records loc record1 record2 =
  let (fields1, _) = List.split (Ast.NameMap.bindings record1) in
  let (fields2, _) = List.split (Ast.NameMap.bindings record2) in

  let missing_fields =
    StringSet.sym_diff (StringSet.of_list fields1) (StringSet.of_list fields2)
  in

  if StringSet.cardinal missing_fields > 0 then begin
    (*Printf.printf "Missing %i fields.\n" (StringSet.cardinal missing_fields);*)
    Error.missing_field loc (StringSet.any missing_fields)
  end;

  Ast.NameMap.iter
    (fun field ty1 ->
      let ty2 = Ast.NameMap.find field record2 in
      unify loc ty1 ty2
    )
    record1

and pairwise_unify loc tys = match tys with
  | [] -> ()
  | [ty] -> ()
  | ty1 :: ty2 :: tys ->
      unify loc ty1 ty2;
      pairwise_unify loc (ty2 :: tys)


(* -------------------- GENERALIZATION & INSTANTIATION -------------------- *)
let rec generalize level ty =
  match ty with
  | TyVar {contents = Unbound (tyvar_id, tyvar_level, None)}
    when tyvar_level > level ->
			TyVar (ref (Generic (tyvar_id, None)))
  | TyVar {contents = Unbound (tyvar_id, tyvar_level, Some traits)}
    when tyvar_level > level ->
			TyVar (ref
        (Generic (tyvar_id, Some (BatDynArray.map (generalize level) traits)))
      )
	| TyVar {contents = Link ty} -> generalize level ty

	| TyCon (name, param_tys) ->
			TyCon (name, List.map (generalize level) param_tys)

	| TyFun (param_tys, ret_ty) ->
			TyFun (List.map (generalize level) param_tys, generalize level ret_ty)

  | TyRecord name_ty_map ->
      TyRecord (Ast.NameMap.map (generalize level) name_ty_map)

	| TyVar {contents = Generic _}
  | TyVar {contents = Unbound _} -> ty

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
		| TyVar {contents = Generic (gen_id, None)} ->
				begin try
					Hashtbl.find generic_to_unbound gen_id
				with Not_found ->
					let tyvar = fresh_tyvar level () in
					Hashtbl.add generic_to_unbound gen_id tyvar;
					tyvar
        end
		| TyVar {contents = Generic (gen_id, Some traits)} ->
				begin try
					Hashtbl.find generic_to_unbound gen_id
				with Not_found ->
          let traits = BatDynArray.map recurse traits in
          let tyvar = fresh_tyvar level ~traits:(Some traits) () in
					Hashtbl.add generic_to_unbound gen_id tyvar;
					tyvar
        end
		| TyVar {contents = Unbound _} as ty -> ty

		| TyCon (name, param_tys) ->
				TyCon (name, List.map recurse param_tys)

		| TyFun (param_tys, ret_ty) ->
				TyFun (List.map recurse param_tys, recurse ret_ty)

		| TyRecord name_ty_map ->
				TyRecord (Ast.NameMap.map recurse name_ty_map)

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

let rec make_ty level kind_env ty =
  match ty with
  | None ->
      (kind_env, fresh_tyvar (level + 1) ())

  | Some (Ast.TyVar (l, id)) ->
      begin match Env.get_opt id kind_env with
        | None ->
            let ty = fresh_tyvar (level + 1) () in
            let kind_env = Env.bind id (KindVar ty) kind_env in
            (kind_env, ty)

        | Some (KindVar ty) ->
            (kind_env, ty)

        | Some (KindFun _) ->
            Error.type_error l
              "Type.make_ty: kind_env is malformed. Maps TyVar to KindFun"
      end

  | Some (Ast.TyCon (l, id, param_tys)) ->
      let (kind_env, param_tys) =
        make_tys level kind_env (List.map (fun x -> Some x) param_tys)
      in

      begin match Env.get_opt id kind_env with
        | None ->
            Error.type_not_found_error l id

        | Some (KindFun ty_fun) ->
            (kind_env, ty_fun param_tys)

        | Some (KindVar _) ->
            Error.type_error l
              "Type.make_params: kind_env is malformed. Maps TyCon to KindVar"
      end

and make_tys level kind_env tys =
  match tys with
  | [] -> (kind_env, [])
  | ty :: rest ->
      let (kind_env, ty) = make_ty level kind_env ty in
      let (kind_env, tys) = make_tys level kind_env rest in
      (kind_env, ty :: tys)

let make_params level kind_env params =
  let (param_names, param_tys) = List.split params in
  let (kind_env, param_tys) = make_tys level kind_env param_tys in
  (kind_env, param_names, param_tys)


let rec typecheck ?level:(level=1) envs ast =
  let ty = infer level envs ast in

  match ast with
    | Ast.Bind (l, mut, id, _) ->
        ({envs with
          ty_env = Env.bind id ty envs.ty_env;
          mut_env = Env.bind id mut envs.mut_env;
        }, ty)

    | Ast.Def (l, id, _, _, _, _) ->
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
        TyFold (None, lazy (TyRecord Ast.NameMap.empty))
      else
        TyFold (None, lazy (TyRecord name_ty_map))

  | Ast.Field (l, ast, name) ->
			let field_ty = fresh_tyvar level () in
      let record_ty = has_field_ty ~level:level name field_ty in
			unify l record_ty (infer level envs ast);
			field_ty

  | Ast.Lambda (l, params, ast) ->
      let (kind_env, param_names, param_tys) =
        make_params level envs.kind_env params
      in
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

  | Ast.SetField (l, ast1, name, ast2) ->
			let field_ty = fresh_tyvar level () in
      let record_ty = has_field_ty ~level:level name field_ty in

      let ast1_ty = infer level envs ast1 in
      let ast2_ty = infer level envs ast2 in

			unify l record_ty ast1_ty;
      unify l field_ty ast2_ty;

      ast2_ty

  (* TODO : do a thorough check of whether or not the level logic is correct *)
  | Ast.Def (l, name, traits, params, ret_ty, suite) ->
      (* TODO: check for duplicates in traits *)
      let functype = fresh_tyvar (level + 1) () in
      let (kind_env', param_names, param_tys) =
        make_params level envs.kind_env params
      in
      let ty_env' =
        Env.bind_many (name :: param_names) (functype :: param_tys) envs.ty_env
      in

      (* none of the parameters, nor the function name is mutable
       * TODO: maybe make function parameters potentially mutable? *)
      let len = List.length (name :: param_names) in
      let mut_env' =
        Env.bind_many
        (name :: param_names)
        (List.init len (fun _ -> false)) envs.mut_env
      in

      let envs' = {
        envs with
          ty_env = ty_env';
          mut_env = mut_env';
          kind_env = kind_env'
      }
      in

      let suite_props = typecheck_suite (level + 1) envs' suite in

      if suite_props.flow_stmts then
        Error.flow_outside_loop l;

      if suite_always_returns suite then
        pairwise_unify l suite_props.ret_tys
      else
        pairwise_unify l (none_ty :: suite_props.ret_tys);

      let (envs, ret_ty_provided) = make_ty level envs'.kind_env ret_ty in

      let ret_ty = match suite_props.ret_tys with
        | [] -> none_ty
        | ret_ty :: _ -> ret_ty
      in

      unify l ret_ty_provided ret_ty;
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

