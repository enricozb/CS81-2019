open Env
(*pen Syntax*)
open Error

type ml_type = TyVar of string
        | TyCon of string
        | TyApp of ml_type * ml_type list

(* TODO should first param be a list or a StringSet? *)
type type_scheme = ForAll of string list * ml_type

let rec eq_ty t t' = match (t, t') with
  | (TyCon c, TyCon c') -> c = c'
  | (TyApp (a, cs), TyApp (a', cs')) -> (eq_ty a a') && (eq_tys cs cs')
  | (TyVar a, TyVar a') -> a = a'
  | _ -> false

and eq_tys ts ts' = List.for_all (fun i -> i) (List.map2 eq_ty ts ts')

(* TODO: This may need to be cleaned up to account for preferences in type output style. *)
let rec render_ty = function
  | TyVar a -> "'" ^ a
  | TyCon a -> a
  | TyApp (TyCon "function", [TyApp (TyCon "arguments", argtys); retty]) ->
    "(" ^ String.concat " " (List.map render_ty argtys) ^ " -> " ^ render_ty retty ^ ")"
  | TyApp (ty, tys) ->
    "(" ^ String.concat " " (List.map render_ty (ty :: tys)) ^ ")"

let render_tyscheme = function
  | ForAll ([], ty) -> render_ty ty
  | ForAll ([a], ty) ->
    "forall '" ^ a ^ " " ^ render_ty ty ^ ""
  | ForAll (vars, ty) ->
    "forall ('" ^ String.concat " '" vars ^ ") " ^ render_ty ty ^ ""

(* ----------------------------------------------------------------------------
 * Type Substitutions
 *)

type subst = ml_type env

let subst_id = empty_env

let subst_var theta t = try raw_find theta t with Not_found -> TyVar t

let rec subst_ty theta = function
  | TyVar a -> subst_var theta a
  | TyCon c -> TyCon c
  | TyApp (a, cs) -> TyApp (a, List.map (subst_ty theta) cs)

let subst_dom theta =
  StringMap.fold (fun key _ set -> StringSet.add key set) theta StringSet.empty

let subst_compose theta_2 theta_1 =
  let domain = StringSet.union (subst_dom theta_1) (subst_dom theta_2) in
  let replace t = subst_ty theta_2 (subst_var theta_1 t) in
    StringSet.fold (fun t theta -> bind theta t (replace t)) domain subst_id

let instantiate_ty (ForAll (formals, ty)) actuals =
  subst_ty (bind_list subst_id formals actuals) ty

(* TODO: Does this need to be ordered? If so, lift diff into a function
* with the signature `string list -> string list -> StringSet` tbh... *)
let free_tyvars t =
  let rec f set = function
    | TyVar a -> StringSet.add a set
    | TyCon _ -> set
    | TyApp (t, ts) -> List.fold_left f (f set t) ts
  in
   f StringSet.empty t

let canonicalize_tyscheme (ForAll (bound, ty)) =
  let rec canonical_tyvar n =
    if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n))
    else "v" ^ string_of_int (n - 25)
  and free = StringSet.diff (free_tyvars ty) (StringSet.of_list bound)
  and unused_idx n =
    if StringSet.mem (canonical_tyvar n) free then unused_idx (n + 1) else n
  and new_bound_vars idx vars = match (idx, vars) with
    | (_, []) -> []
    | (idx, _ :: oldvars) ->
      let n = unused_idx idx in
        canonical_tyvar n :: new_bound_vars (n + 1) oldvars in
  let new_bound = new_bound_vars 0 bound in
  let new_subst = bind_list subst_id bound (List.map (fun t -> TyVar t) new_bound)
  in
    ForAll (new_bound, subst_ty new_subst ty)

(* ----------------------------------------------------------------------------
 * Type Basis, p 487.
 *)

let num_ty = TyCon "int"
let bool_ty = TyCon "bool"
let sym_ty = TyCon "sym"
let unit_ty = TyCon "unit" (* TODO: is unit really a type? I thought '() was a list... *)
let list_ty ty = TyApp (TyCon "list", [ty])
let ref_ty ty = TyApp (TyCon "ref", [ty])
let pair_ty x y = TyApp (TyCon "pair", [x; y])
let funtype_of argtys retty =
  TyApp (TyCon "function", [TyApp (TyCon "arguments", argtys); retty])

let canonicalize_together a b =
  let united_ty = pair_ty a b in
  let vars = StringSet.elements (free_tyvars united_ty) in
    match canonicalize_tyscheme (ForAll (vars, united_ty)) with
      | ForAll (_, TyApp (TyCon "pair", [a; b])) -> (a, b)
      | _ -> raise NanoML_NeverHappen_err

(* ----------------------------------------------------------------------------
 * Type variables and type environments.
 *)

type type_env = type_scheme env * StringSet.t

let empty_tyenv = (empty_env, StringSet.empty)

(* TODO: error case? Also gamma is the worst name here,
 * since the whole tuple is also called gamma elsewhere.... *)
let tyenv_find l (gamma, free) x = find l gamma x

let tyenv_bind (gamma, free) x (ForAll (bound, ty) as sigma) =
  let new_free = StringSet.diff (free_tyvars ty) (StringSet.of_list bound) in
    (bind gamma x sigma, StringSet.union new_free free)

let rec fresh_counter = ref 0
and fresh_tyvar _ =
    (fresh_counter := !fresh_counter + 1;
      TyVar ("'t" ^ string_of_int !fresh_counter))
and fresh_conc_ty _ =
    (fresh_counter := !fresh_counter + 1;
      TyCon ("<concrete " ^ string_of_int !fresh_counter ^ ">"))

let free_tyvars_tyenv (_, free) = free

(* TODO: All of these names should be standardized... *)
let generalize_ty ty tyvars =
  canonicalize_tyscheme
    (ForAll (StringSet.elements (StringSet.diff (free_tyvars ty) tyvars), ty))

let generalize_free ty =
  generalize_ty ty StringSet.empty

let fresh_tyenv (ForAll (bound, ty)) =
  instantiate_ty (ForAll (bound, ty)) (List.map fresh_tyvar bound)

(* ----------------------------------------------------------------------------
 * Constraints.
 *)

(* TODO: are these the best name for this? *)
type con = ConEq of ml_type * ml_type
         | ConAnd of con * con
         | ConTrivial

let rec render_con = function
  | ConEq (t, t') -> "(" ^ render_ty t ^ ") ~ (" ^ render_ty t' ^ ")"
  | ConAnd (c, c') -> render_con c ^ " /\\ " ^ render_con c'
  | ConTrivial -> "T"

let rec free_tyvars_con = function
  | ConEq (t, t') -> StringSet.union (free_tyvars t) (free_tyvars t')
  | ConAnd (c, c') -> StringSet.union (free_tyvars_con c) (free_tyvars_con c')
  | ConTrivial -> StringSet.empty

let rec subst_con theta = function
  | ConEq (t, t') -> ConEq (subst_ty theta t, subst_ty theta t')
  | ConAnd (c, c') -> ConAnd (subst_con theta c, subst_con theta c')
  | ConTrivial -> ConTrivial

let rec con_eq_tys = function
  | [] -> ConTrivial
  | [ty] -> ConTrivial
  (* ty1 = ty2 AND equate (ty2 :: tys) *)
  | ty1 :: ty2 :: tys -> ConAnd (ConEq (ty1, ty2), con_eq_tys (ty2 :: tys))

let rec con_join = function
  | [] -> ConTrivial
  | [c] -> c
  | c :: cs -> ConAnd (c, con_join cs)

let rec con_solve l c = match c with
  | ConTrivial -> subst_id
  | ConAnd (c1, c2) ->
    let theta1 = con_solve l c1 in
    let theta2 = con_solve l (subst_con theta1 c2) in
      subst_compose theta2 theta1
  | ConEq (t1, t2) -> match (t1, t2) with
    | (TyVar a, TyVar b) when a = b -> subst_id
    | (TyCon a, TyCon b) when a = b -> subst_id

    | (TyVar a, b) ->
      let vars = free_tyvars b in
        if StringSet.mem a vars then
          (let (a', b') = canonicalize_together (TyVar a) b in
            Error.type_err l ~expected:(render_ty a') ~found:(render_ty b'))
        else
          bind subst_id a b
    | (a, TyVar b) -> con_solve l (ConEq (TyVar b, a))

    | (TyApp (t, ts), TyApp (t', ts'))
        when (List.length ts) = (List.length ts') ->
      let con_args = List.map2 (fun a b -> ConEq (a, b)) ts ts' in
        con_solve l (con_join (ConEq (t, t') :: con_args))

    | (a, b) ->
      let (a', b') = canonicalize_together a b in
        Error.type_err l ~expected:(render_ty a') ~found:(render_ty b')

(* ----------------------------------------------------------------------------
 * Generality checking for check-*.
 *)

let ty_as_general_as l sigma1 (ForAll (vars, ty)) =
  let theta = bind_list subst_id vars (List.map fresh_conc_ty vars) in
  let concrete_ty = subst_ty theta ty in
  let fresh_sigma1 = fresh_tyenv sigma1 in
  try
    ignore @@ con_solve l (ConEq (concrete_ty, fresh_sigma1));
    true
  with Error.NanoML_err e -> false
