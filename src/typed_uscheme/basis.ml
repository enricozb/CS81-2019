(*
 * The uscheme initial basis.
 *)

open Env

(* Runtime type errors are illegal; we don't handle them. *)
let panic () = failwith "fatal: runtime type error"

(* Basics. *)
let unary_fun f =
  PrimFuncVal (fun vals loc ->
    match vals with
    | [a] -> f a loc
    | _ -> panic ())

let binary_fun f =
  PrimFuncVal (fun vals loc ->
    match vals with
    | [a; b] -> f a b loc
    | _ -> panic ())

(* Integer operations. *)
let binary_int_fun f =
  binary_fun (fun a b loc ->
    match (a, b) with
    | (IntVal a, IntVal b) -> f a b
    | _ -> panic ())

let binary_int_int_fun f = binary_int_fun (fun x y -> IntVal (f x y))
let int_comparison f = binary_int_fun (fun x y -> BoolVal (f x y))

(* List functions. *)
let cons = binary_fun (fun a b _ -> PairVal (a, b))

let car =
  unary_fun (fun v loc ->
    match v with
    | PairVal (a, b) -> a
    | NilVal -> Error.runtime_err "car of empty list"
    | _ -> panic ())

let cdr =
  unary_fun (fun v loc ->
    match v with
    | PairVal (a, b) -> b
    | NilVal -> Error.runtime_err "cdr of empty list"
    | _ -> panic ())

let is_nil =
  unary_fun (fun v loc ->
    match v with
    | NilVal -> BoolVal true
    | PairVal (_, _) -> BoolVal false
    | _ -> panic ())

(* "Safe" division--throws a Uscheme_error on division by zero. *)
let safe_div a b =
  try
    a / b
  with Division_by_zero ->
    Error.runtime_err "division by zero"

let basis_vals = make_env
  [("#t", BoolVal true);
   ("#f", BoolVal false);
   ("#u", UnitVal);
   ("nil", NilVal);
   ("+", binary_int_int_fun (+));
   ("*", binary_int_int_fun ( * ));
   ("/", binary_int_int_fun safe_div);
   ("-", binary_int_int_fun (-));
   (">", int_comparison (>));
   ("<", int_comparison (<));
   (">=", int_comparison (>=));
   ("<=", int_comparison (<=));
   ("=", int_comparison (=));
   ("cons", cons);
   ("car", car);
   ("cdr", cdr);
   ("null?", is_nil)]

let bool_ty   = Type.bool_ty
let int_ty    = Type.int_ty
let unit_ty   = Type.unit_ty
let list_of t = Syntax.TyApp (Syntax.TyCon "list", [t])

let binary_int_int_sig = Syntax.FunctionType ([int_ty; int_ty], int_ty)
let int_comparison_sig = Syntax.FunctionType ([int_ty; int_ty], bool_ty)

(* Shorthand for "(forall 'a ty)". *)
let forall ty = Syntax.Forall (["a"], ty)

let cons_sig =
  let a = Syntax.TyVar "a" in
  forall @@ Syntax.FunctionType ([a; list_of a], list_of a)

let car_sig =
  let a = Syntax.TyVar "a" in
  forall @@ Syntax.FunctionType ([list_of a], a)

let cdr_sig =
  let a = Syntax.TyVar "a" in
  forall @@ Syntax.FunctionType ([list_of a], list_of a)

let is_nil_sig =
  let a = Syntax.TyVar "a" in
  forall @@ Syntax.FunctionType ([list_of a], bool_ty)

let basis_types = Type.make_env
  (* (Gamma) Type environment *)
  [("#t", bool_ty);
   ("#f", bool_ty);
   ("#u", unit_ty);
   ("nil", forall (list_of @@ Syntax.TyVar "a"));
   ("+", binary_int_int_sig);
   ("*", binary_int_int_sig);
   ("/", binary_int_int_sig);
   ("-", binary_int_int_sig);
   (">", int_comparison_sig);
   ("<", int_comparison_sig);
   (">=", int_comparison_sig);
   ("<=", int_comparison_sig);
   ("=", int_comparison_sig);
   ("cons", cons_sig);
   ("car", car_sig);
   ("cdr", cdr_sig);
   ("null?", is_nil_sig)]

  (* (Delta) Kind environment *)
  [("bool", Type.Type);
   ("int", Type.Type);
   ("unit", Type.Type);
   ("list", Type.Arrow ([Type.Type], Type.Type))]

let scheme_basis = ""
