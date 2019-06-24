(* -------------------- Exceptions Internal to type.ml -------------------- *)
type ty_error =
  | MissingField of string

exception TypeCheckingError of ty_error

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
  | TyFold of ((id * (ty list)) option) * (ty Lazy.t) (* for recursive types *)
  | TyUnfold of ty

  (* this really shouldn't exist... It's used only in unify *)
  (* it is a trait that is not backed by a full record type,
   * and is only *)
  | TyTraitCon of (string, ty list) BatHashtbl.t

and tyvar =
  | Link of ty
  | Unbound of id * level * (trait option)
  | Generic of id * (trait option)

and kind =
  | KindFun of kind_fun
  | KindVar of ty
  | KindTrait of kind_trait

and kind_fun = (ty list) -> ty
and kind_trait = level -> ty -> (ty list) -> ty

(* a mutable TyFold *)
and trait = ((string, ty list) BatHashtbl.t) * (ty Lazy.t)

and tyrow = ty (* kind of rows should only be TyRowEmpty or TyRowExtend *)


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

let rec real_ty ty =
  match ty with
  | TyVar {contents = Link ty} -> let x = real_ty ty in x
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
let string_of_type ty =
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

  and string_of_trait trait =
    let (name_params_map, _) = trait in
    let trait_strs =
      List.map
        (fun (name, params) ->
          let param_strs = List.map recurse params in
          if param_strs = [] then
            name
          else
            name ^ "[" ^ (String.concat ", " param_strs) ^ "]")
        (List.sort compare @@ BatHashtbl.bindings name_params_map)
    in
    String.concat " + " trait_strs

  and recurse ?(toplevel=false) ty =
    match ty with
    | TyVar {contents = Generic (id, trait)} ->
        begin try
          BatHashtbl.find id_to_char id
        with Not_found ->
          let newid = next_char () in
          BatHashtbl.add id_to_char id newid;

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
          begin match trait with
          | Some trait ->
            BatHashtbl.add id_to_traits newid (string_of_trait trait)
          | None -> ()
          end;

          newid
        end

    | TyVar {contents = Unbound (id, _, None)} ->
        "_" ^ id
    | TyVar {contents = Unbound (id, _, Some trait)} ->
        let id = "_" ^ id in
        begin try
          BatHashtbl.find id_to_char id
        with Not_found ->
          BatHashtbl.add id_to_char id id;
          BatHashtbl.add id_to_traits id (string_of_trait trait);
          id
        end

    | TyVar {contents = Link ty} ->
        recurse (real_ty ty)

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

    | TyFold (Some (id, []), _) ->
        (*"FOLD(" ^ id ^ ")"*)
        id
    | TyFold (Some (id, param_tys), _) ->
        (*"FOLD(" ^ id ^ "[" ^ (recurse_tys param_tys) ^ "])"*)
        id ^ "[" ^ (recurse_tys param_tys) ^ "]"

    | TyFold (None, ty) ->
        (*"FOLD(" ^ recurse (Lazy.force ty) ^ ")"*)
        recurse (Lazy.force ty)

    | TyUnfold (ty1) ->
        begin match real_ty ty1 with
          | TyFold (_, ty1) ->
              recurse (Lazy.force ty1)
          | ty1 ->
              "UNFOLD(" ^ recurse ty1 ^ ")"
        end

    | TyTraitCon trait_con ->
        string_of_trait (trait_con, lazy (TyCon ("~ty-trait-con~INTERNAL", [])))

  in
  let ty_str = recurse ~toplevel:true ty in
  let trait_bindings = List.sort compare (BatHashtbl.bindings id_to_traits) in
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
  let seen_tyvars = BatHashtbl.create 10 in
  let rec recurse ty =
    match ty with
      | TyVar {contents = Generic _} ->
          failwith ("Type.occurs: type '" ^ (string_of_type ty) ^ "' is Generic.")

      | TyVar ({contents = Unbound (tyvar_id2, tyvar_level2, trait)} as tyvar2) ->

        if BatHashtbl.mem seen_tyvars tyvar_id2 then
          ()
        else begin
          BatHashtbl.add seen_tyvars tyvar_id2 true;
          (*
           * Because we allow recursive traits like <a: Addable[a, a]>,
           * we don't check the trait at all
           *)
          if tyvar_id = tyvar_id2 then
            Error.type_error loc "recursive types"
          else
            if tyvar_level2 > tyvar_level then
              tyvar2 := Unbound (tyvar_id2, tyvar_level, trait)
        end
      | TyVar {contents = Link ty} ->
          recurse (real_ty ty)

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

      | TyFold (Some (id, param_tys), _) ->
          List.iter recurse param_tys

      (* to allow recursive checks, aka
       *    does `a` occur in {x: FOLD({y: a | b})}
       *)
      | TyFold (None, ty)
      | TyUnfold (TyFold (_, ty)) ->
          recurse (Lazy.force ty)

      | TyUnfold _ ->
          failwith "Type.occurs on bare TyUnfold"
  in
  recurse ty

let rec fresh_counter = ref 0
and fresh_tyvar level ?(trait=None) _ =
  incr fresh_counter;
  TyVar {contents = Unbound ("t" ^ string_of_int !fresh_counter, level, trait)}

and fresh_gen_tyvar ?(trait=None) _ =
  incr fresh_counter;
  TyVar {contents = Generic ("t" ^ string_of_int !fresh_counter, trait)}

and make_tyvar level generic ?(trait=None) () =
  if generic then
    fresh_gen_tyvar ~trait:trait ()
  else
    fresh_tyvar level ~trait:trait ()

let make_singleton_trait name param_tys record_ty =
  let name_params_map = BatHashtbl.create 1 in
  BatHashtbl.add name_params_map name param_tys;
  (name_params_map, record_ty)

(* ----------------------------- COMMON TYPES ----------------------------- *)
(* initialized in basis.ml *)
let function_class_ty = ref (TyCon ("FakeFunctionClass", []))
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

(* to be filled with some stuff eventually... *)
let rec base_record_fields () = Ast.NameMap.empty

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
    ("__name__", !string_ty);
    ("__class__", !function_class_ty);
    ("__call__", func_type);
    ("~~call~~", prim_fun_ty)
  ])
  and func_type =
    TyFold (Some ("Function", param_tys @ [ret_ty]), inner_record)
  in
  func_type

let callable_trait ?(level=0) ?(generic=false) param_tys ret_ty =
  let record_ty = lazy (TyRecord (
      TyRowExtend (
        Ast.NameMap.singleton "__call__" (fun_ty param_tys ret_ty),
        make_tyvar level generic ()
      )
    ))
  in
  let trait =
    make_singleton_trait "Callable" (param_tys @ [ret_ty]) record_ty
  in

  make_tyvar level generic ~trait:(Some trait) ()

let extensible_field_ty ?(level=0) ?(generic=false) field field_ty =
  TyRecord (
    TyRowExtend (
      Ast.NameMap.singleton field field_ty,
      make_tyvar level generic ()
    )
  )

let has_field_trait ?(level=0) ?(generic=false) field field_ty =
  let record_ty =
    lazy (extensible_field_ty ~level:level ~generic:generic field field_ty)
  in
  let trait =
    make_singleton_trait ("{" ^ field ^ "}") [field_ty] record_ty
  in
  make_tyvar level generic ~trait:(Some trait) ()

(* ------------------------------ UNIFICATION ------------------------------ *)

let can_unfold ty =
  match ty with
  | TyVar {contents = Unbound (_, _, Some _)}
  | TyFold _ ->
      true
  | _ ->
      false

let unfold loc ty =
  match ty with
  | TyVar {contents = Unbound (_, _, Some trait)} ->
      let (_, record_ty) = trait in Lazy.force record_ty
  | TyFold (_, record_ty) ->
      Lazy.force record_ty
  | _ ->
      Error.implementation_error
        ~loc: loc
        ("Type.unfold: Called on non-unfoldable: " ^ (string_of_type ty))

let rec unify loc ?(top_tys=None) ty1 ty2 =
  (*Printf.printf "unifying: \n\t%s\n\n\t%s\n\n" (string_of_type ty1) (string_of_type ty2);*)
  let rec recurse ?(top_tys=None) ty1 ty2 =
    if ty1 == ty2 then
      ()
    else
      match (ty1, ty2) with
      | TyCon (name1, ty_params1), TyCon (name2, ty_params2) ->
          if name1 = name2 && List.length ty_params1 = List.length ty_params2 then
            List.iter2 recurse ty_params1 ty_params2
          else
            Error.unify_error loc (string_of_type ty1) (string_of_type ty2)

      | TyVar {contents = Link ty1}, ty2
      | ty1, TyVar {contents = Link ty2} ->
          recurse ty1 ty2

      (* non-trait bounded type variable *)
      | TyVar ({contents = Unbound (tyvar_id, tyvar_level, None)} as ty_ref), ty
      | ty, TyVar ({contents = Unbound (tyvar_id, tyvar_level, None)} as ty_ref) ->
          (* TODO: maybe catch the MythError and raise an error specific to
           * the two types `ty1` and `ty2`? *)
          occurs loc tyvar_id tyvar_level ty;
          ty_ref := Link ty

      (* unifying two trait bounded type variables *)
      | TyVar ({contents = Unbound (id1, level1, Some trait1)} as ty_ref1),
        TyVar {contents = Unbound (id2, level2, Some trait2)} ->
          if id1 == id2 then
            ()
          else begin
            let (name_params_map1, record_ty1) = trait1 in
            let (name_params_map2, record_ty2) = trait2 in

            (* should this happen? this might prevent recursive traits *)
            (*List.iter (occurs loc id1 level1) !params2;*)
            (*List.iter (occurs loc id2 level2) !params1;*)

            (* eliminate ty_ref1. Do this before recursive call so that recursive
             * structures hopefully don't infinite loop. Recursive usage of a
             * type variable will have been replaced already, I hope *)
            ty_ref1 := Link ty2;

            (* this as top_tyes makes missing field errors clearer *)
            let top_tys =
              (TyTraitCon name_params_map1, TyTraitCon name_params_map2)
            in
            recurse
              ~top_tys:(Some top_tys)
              (Lazy.force record_ty1)
              (Lazy.force record_ty2);

            (* TODO: changing both TyTraitCons...
             * Might just need to change one of them? *)
            BatHashtbl.iter
              (fun name params ->
                if not (BatHashtbl.mem name_params_map2 name) then
                  BatHashtbl.add name_params_map2 name params
              )
              name_params_map1;

            BatHashtbl.iter
              (fun name params ->
                if not (BatHashtbl.mem name_params_map1 name) then
                  BatHashtbl.add name_params_map1 name params
              )
              name_params_map2;
          end

      (* unifying a trait bounded type variable with a non-type variable *)
      | TyVar ({contents = Unbound (tyvar_id, tyvar_level, Some trait)} as ty_ref), (TyFold (_, record_ty) as fold_ty)
      | (TyFold (_, record_ty) as fold_ty), TyVar ({contents = Unbound (tyvar_id, tyvar_level, Some trait)} as ty_ref) ->

          let (name_params_map, trait_record) = trait in

          (* eliminate the type variable. Do this before recursive call so that
           * recursive structures hopefully don't infinite loop. Recursive usage
           * of a type variable will have been replaced already, I hope. *)
          occurs loc tyvar_id tyvar_level fold_ty;
          ty_ref := Link fold_ty;

          (* the order of these types must match the order in the `recurse` call below *)
          let top_tys = (TyTraitCon name_params_map, fold_ty) in

          (* make sure `ty` conforms to the trait contents *)
          recurse ~top_tys:(Some top_tys) (Lazy.force trait_record) (Lazy.force record_ty)

          (* TODO : trait no longer exists, do we need to do anything to it? *)

      | TyFun (ty_params1, ty_ret1), TyFun (ty_params2, ty_ret2) ->
          (* TODO : improve this error... it's often just a simple
           * call error, where the incorrect number of arguments were
           * provided *)
          if List.length ty_params1 <> List.length ty_params2 then
            Error.unify_error loc (string_of_type ty1) (string_of_type ty2);

          (* return types should be equal, as should argument types *)
          List.iter2 recurse ty_params1 ty_params2;
          recurse ty_ret1 ty_ret2

      | TyRecord tyrow1, TyRecord tyrow2 ->
          recurse ~top_tys:top_tys tyrow1 tyrow2

      | TyRowEmpty, TyRowEmpty -> ()

      | (TyRowExtend _ as tyrow1), (TyRowExtend _ as tyrow2) ->
          unify_rows loc top_tys tyrow1 tyrow2

      | TyRowEmpty, TyRowExtend (name_ty_map, _) ->
          let field, _ = Ast.NameMap.choose name_ty_map in
          begin match top_tys with
          | None ->
            Error.missing_field_for_type loc ("UNKNOWN TYPE, MISSING TOP_TYS!") field
          | Some (top_ty1, _)->
            Error.missing_field_for_type loc (string_of_type top_ty1) field
          end

      | TyRowExtend (name_ty_map, _), TyRowEmpty ->
          let field, _ = Ast.NameMap.choose name_ty_map in
          begin match top_tys with
          | None ->
            Error.missing_field_for_type loc ("UNKNOWN TYPE, MISSING TOP_TYS!") field
          | Some (_, top_ty2)->
            Error.missing_field_for_type loc (string_of_type top_ty2) field
          end

      | TyFold (Some (id1, param_tys1), _),
        TyFold (Some (id2, param_tys2), _) ->
          recurse (TyCon (id1, param_tys1)) (TyCon (id2, param_tys2))

      | TyFold (None, ty1), TyFold (None, ty2) ->
          recurse (Lazy.force ty1) (Lazy.force ty2)

      (* useful when unifying something like
       *
       *    Unfold(_t1) = Unfold(Fold({...}))
       *
       * so, when we have a unfolded type variable.
       *
       * The `if` is so if we have two trait-bounded type variables being
       * unified with Unfolds, we actually get their contents, not just
       * the trait-bounded type variables.
       *)
      | TyUnfold (ty1), TyUnfold (ty2) ->
          (*if can_unfold ty1 && can_unfold ty2 then*)
            (*recurse (unfold loc ty1) (unfold loc ty2)*)
          (*else*)
            recurse ty1 ty2

      | TyUnfold (ty1), ty2
      | ty2, TyUnfold (ty1) ->
          recurse (unfold loc ty1) ty2;

      | TyFold (Some (name1, params1), _), TyCon (name2, params2)
      | TyCon (name1, params1), TyFold (Some (name2, params2), _) ->
          recurse (TyCon (name1, params1)) (TyCon (name2, params2))

      | (ty1, ty2) ->
          Error.unify_error loc (string_of_type ty1) (string_of_type ty2)
    in
    recurse ~top_tys:top_tys ty1 ty2


and unify_rows loc top_tys tyrow1 tyrow2 =
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
    | (true, true) -> unify loc ~top_tys:top_tys rest_ty1 rest_ty2
    | (true, false) -> unify loc ~top_tys:top_tys (TyRowExtend (missing2, rest_ty1)) rest_ty2
    | (false, true) -> unify loc ~top_tys:top_tys rest_ty1 (TyRowExtend (missing1, rest_ty2))
    | (false, false) ->
        begin match rest_ty1 with
          | TyRowEmpty ->
              (* will result in an error *)
              unify loc ~top_tys:top_tys rest_ty1 (TyRowExtend (missing1, fresh_tyvar 0 ()))

          (* trait is always None since this is an extend type *)
          | TyVar ({contents = Unbound (_, level, None)} as ty_ref) ->
              let new_rest_row_var = fresh_tyvar level () in
              unify loc ~top_tys:top_tys (TyRowExtend (missing2, new_rest_row_var)) rest_ty2 ;
              begin match !ty_ref with
                | Link _ -> Error.type_error loc "recursive types"
                | _ -> ()
              end;
              unify loc ~top_tys:top_tys rest_ty1 (TyRowExtend (missing1, new_rest_row_var))

          | TyVar {contents = Unbound (_, level, _)} ->
             failwith "Type.unify_rows extend tyvar has non-None trait"

          | _ -> assert false
        end

let rec pairwise_unify loc tys = match tys with
  | [] -> ()
  | [ty] -> ()
  | ty1 :: ty2 :: tys ->
      unify loc ty1 ty2;
      pairwise_unify loc (ty2 :: tys)


(* -------------------- GENERALIZATION & INSTANTIATION -------------------- *)
let generalize level ty =
  (* used for recursive types like <a: Iterable[a]> *)
  let unbound_to_generic = BatHashtbl.create 10 in

  let rec recurse = function
    | TyVar {contents = Unbound (tyvar_id, tyvar_level, None)}
      when tyvar_level > level ->
        TyVar (ref (Generic (tyvar_id, None)))

    | TyVar {contents = Unbound (tyvar_id, tyvar_level, Some trait)}
      when tyvar_level > level ->

        begin match BatHashtbl.find_option unbound_to_generic tyvar_id with
          | None ->
              let temp_tyvar_ref = ref (Generic ("temp~" ^ tyvar_id, None)) in
              BatHashtbl.add unbound_to_generic tyvar_id temp_tyvar_ref;

              let (name_params_map, record_ty) = trait in

              let name_params_map =
                BatHashtbl.map
                  (fun _ params -> List.map recurse params)
                  name_params_map
              in
              let record_ty = lazy (recurse (Lazy.force record_ty)) in

              let trait = (name_params_map, record_ty) in
              temp_tyvar_ref := Generic (tyvar_id, Some trait);
              TyVar (temp_tyvar_ref)
          | Some tyvar_ref ->
              TyVar tyvar_ref
        end

    | TyVar {contents = Link ty} -> recurse ty

    | TyCon (name, param_tys) ->
        TyCon (name, List.map recurse param_tys)

    | TyFun (param_tys, ret_ty) ->
        TyFun (List.map recurse param_tys, recurse ret_ty)

    | TyRecord tyrow -> TyRecord (recurse tyrow)

    | TyRowExtend (name_ty_map, rest_ty) ->
          TyRowExtend (Ast.NameMap.map recurse name_ty_map, recurse rest_ty)

    | TyVar {contents = Generic _}
    | TyVar {contents = Unbound _}

    | TyRowEmpty as ty -> ty

    | TyFold (None, ty) ->
        TyFold (None, lazy (recurse (Lazy.force ty)))
    | TyFold (Some (id, param_tys), ty) ->
        TyFold (Some (id, List.map recurse param_tys),
                lazy (recurse @@ Lazy.force ty))

    | TyUnfold _ ->
        failwith "Type.generalize on TyUnfold"
  in
  recurse ty

let rec instantiate level ty =
  let generic_to_unbound = BatHashtbl.create 10 in

  let rec recurse = function
    | TyVar {contents = Link ty} -> recurse ty
    | TyVar {contents = Generic (gen_id, None)} ->
        begin try
          BatHashtbl.find generic_to_unbound gen_id
        with Not_found ->
          let tyvar = fresh_tyvar level () in
          BatHashtbl.add generic_to_unbound gen_id tyvar;
          tyvar
        end

    | TyVar {contents = Generic (gen_id, Some trait)} ->
        begin match BatHashtbl.find_option generic_to_unbound gen_id with
          | None ->
              (* this will be replaced at the end of this match arm *)
              let temp_tyvar_ref =
                (ref (Unbound ("temp~" ^ gen_id, -1, None)))
              in
              let temp_tyvar = TyVar temp_tyvar_ref in

              BatHashtbl.add generic_to_unbound gen_id temp_tyvar;

              let (name_params_map, record_ty) = trait in

              let name_params_map =
                BatHashtbl.map
                  (fun _ params -> List.map recurse params)
                  name_params_map
              in
              let record_ty = lazy (recurse (Lazy.force record_ty)) in
              let trait = (name_params_map, record_ty) in

              begin match fresh_tyvar level ~trait:(Some trait) () with
                | TyVar {contents = tyvar} ->
                    temp_tyvar_ref := tyvar
                | _ ->
                    assert false
              end;
              temp_tyvar

          | Some tyvar ->
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


(* ------------------------------ ANNOTATIONS ------------------------------ *)
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

        | Some _ ->
            Error.type_error l
              "Type.make_ty: kind_env malformed. Maps TyVar to non-KindVar"
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

        | Some _ ->
            Error.type_error l
              "Type.make_params: kind_env malformed. Maps TyCon to non-KindFun"
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

let apply_trait level trait kind_env =
  let (tyvar, trait) = trait in

  let (kind_env, trait_name, trait_params, trait_fun) =
    match trait with
    | Ast.TyCon (l, id, param_tys) ->
        let (kind_env, param_tys) =
          make_tys level kind_env (List.map (fun x -> Some x) param_tys)
        in
        begin match Env.get_opt id kind_env with
          | None ->
              Error.trait_not_found_error l id
          | Some (KindTrait trait_fun) ->
              (kind_env, id, param_tys, (fun ty -> trait_fun level ty param_tys))
          | Some _ ->
              Error.type_error l ("Type '" ^ id ^ "' is not a trait.")
        end
    | _ ->
        failwith "Type.apply_trait: trait is type variable."
  in

  let (loc, kind_env, tyvar) =
    match tyvar with
    | Ast.TyVar (l, id) ->
      begin match Env.get_opt id kind_env with
      | Some (KindVar tyvar) ->
          (l, kind_env, tyvar)
      | None ->
          let tyvar = fresh_tyvar (level + 1) () in
          let kind_env = Env.bind id (KindVar tyvar) kind_env in
          (l, kind_env, tyvar)
      | _ ->
          failwith "Type.apply_trait: kind_env malformed (TyVar => non-KindVar)."
      end

    | _ ->
        failwith "Type.apply_trait: Trying to bound non-type variable."
  in

  match tyvar with
  | TyVar ({contents = Unbound (id, level, None)} as tyvar_ref) ->
      let record_ty = lazy (trait_fun (TyVar tyvar_ref)) in
      let trait = make_singleton_trait trait_name trait_params record_ty in
      tyvar_ref := Unbound (id, level, Some trait);
      kind_env
  | TyVar ({contents = Unbound (id, _, Some _)}) ->
      Error.syntax_error loc ("Trait '" ^ id ^ "' listed twice.")
  | _ ->
      failwith "Type.apply_trait: kind_env malformed (KindVar (non-TyVar))."

let rec apply_traits level traits kind_env =
  match traits with
  | [] -> kind_env
  | trait :: rest ->
      apply_traits level rest (apply_trait level trait kind_env)

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
        TyFold (None, lazy (TyRecord TyRowEmpty))
      else
        TyFold (None, lazy (TyRecord (TyRowExtend (name_ty_map, TyRowEmpty))))

  | Ast.Field (l, ast, field_name) ->
      (*Printf.printf "handling access of '%s'\n" field_name;*)
      let field_ty = fresh_tyvar level () in
      let record_ty = has_field_trait ~level:level field_name field_ty in
      let ast_ty = infer level envs ast in

      begin try
        unify l (TyUnfold record_ty) (TyUnfold ast_ty)
      with
      | TypeCheckingError (MissingField field) ->
          Error.missing_field_for_type l (string_of_type ast_ty) (field)
      end;

      (*Printf.printf "handled access of '%s'\n" field_name;*)
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
      unify l t1 (callable_trait ~level:level param_tys ret_ty);
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

  | Ast.SetField (l, ast1, field_name, ast2) ->
      let field_ty = fresh_tyvar level () in
      let record_ty = has_field_trait ~level:level field_name field_ty in
      let ast1_ty = infer level envs ast1 in
      let ast2_ty = infer level envs ast2 in

      unify l (TyUnfold record_ty) (TyUnfold ast1_ty);
      unify l field_ty ast2_ty;

      ast2_ty

  (* TODO : do a thorough check of whether or not the level logic is correct *)
  | Ast.Def (l, name, traits, params, ret_ty, suite) ->
      let functype = fresh_tyvar (level + 1) () in
      let (kind_env', param_names, param_tys) =
        make_params level envs.kind_env params
      in

      (* modify the type variables so they have trait bounds *)
      let kind_env' = apply_traits (level + 1) traits kind_env' in

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

