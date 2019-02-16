open Env
open Syntax
open Type
open Error

(* Truthy should only evaluate a boolean. *)
let truthy l = function
  | Bool v -> v
  | _ -> raise NanoML_NeverHappen_err

let unary_fun f = Primitive (fun vals loc -> match vals with
    | [a] -> f a loc
    | other -> Error.call_err loc ~expected:1 ~found:(List.length other))

let binary_fun f = Primitive (fun vals loc -> match vals with
    | [a; b] -> f a b loc
    | other -> Error.call_err loc ~expected:2 ~found:(List.length other))

let binary_num_fun f = binary_fun (fun a b loc -> match (a, b) with
    | (Num a, Num b) -> f a b
    | _ -> Error.type_err loc ~expected:"two numbers"
             ~found:(render_val a ^ " and " ^ render_val b))

let num_fun_to_num f = binary_num_fun (fun x y -> Num (f x y))
let num_fun_to_bool f = binary_num_fun (fun x y -> Bool (f x y))

let cons = binary_fun (fun a b _ -> Pair (a, b))
let car = unary_fun (fun v loc -> match v with
    | Pair (a, b) -> a
    | _ -> Error.type_err loc ~expected:"pair" ~found:(render_val v))
let cdr = unary_fun (fun v loc -> match v with
    | Pair (a, b) -> b
    | _ -> Error.type_err loc ~expected:"pair" ~found:(render_val v))

let refer = unary_fun (fun a _ -> Ref (ref a))
let deref = unary_fun (fun a loc -> match a with
    | Ref a -> !a
    | _ -> Error.type_err loc ~expected:"ref" ~found:(render_val a))
let set = binary_fun (fun a b loc -> match a with
    | Ref a -> (a := b; b)
    | _ -> Error.type_err loc ~expected:"ref" ~found:(render_val a))

let print =
  unary_fun (fun v loc -> Printf.printf "%s\n" (render_val v); Nil)

let error =
  unary_fun (fun v loc -> Error.execution_err loc (render_val v))

let eq = binary_fun (fun a b loc -> Bool (match (a, b) with
    | (Nil, Nil) -> true
    | (Num a, Num b) -> a = b
    | (Bool a, Bool b) -> a = b
    | (Sym a, Sym b) -> a = b
    (* TODO: Handle pair, closure, etc. in error? *)
    | _ -> Error.type_err loc
      ~expected:"comparable" ~found:(render_val a ^ " and " ^ render_val b)))

let fst = unary_fun (fun v loc -> match v with
    | Pair (a, b) -> a
    | _ -> Error.type_err loc ~expected:"pair" ~found:(render_val v))

let snd = unary_fun (fun v loc -> match v with
    | Pair (a, b) -> a
    | _ -> Error.type_err loc ~expected:"pair" ~found:(render_val v))

let is_null = unary_fun (fun e l -> match e with
  | Nil -> Bool true
  | _ -> Bool false)

let prim_env = bind_pairs empty_env
  [("null?", is_null);

   ("+", num_fun_to_num (+));
   ("*", num_fun_to_num ( * ));
   ("/", num_fun_to_num ( / ));
   ("-", num_fun_to_num (-));
   (">", num_fun_to_bool (>));
   ("<", num_fun_to_bool (<));
   ("=", eq);

   ("#f", Bool false);
   ("#t", Bool true);
   ("nil", Nil);

   ("cons", cons);
   ("car", car);
   ("cdr", cdr);

   ("ref", refer);
   ("deref", deref);
   ("set", set);

   ("pair", cons);
   ("fst", fst);
   ("snd", snd);

   ("print", print);
   ("error", error);
  ]

(* TODO: these are gross. Need to fix and make prettier. *)
let alpha = TyVar "a"
let alpha_list = list_ty alpha
let alpha_ref = ref_ty alpha
let beta = TyVar "b"

let unary_fun_sig a b = funtype_of [a] b
let binary_fun_sig a b = funtype_of [a; a] b

let prim_sigs =
  [("null?", unary_fun_sig alpha_list bool_ty);

   ("+", binary_fun_sig num_ty num_ty);
   ("*", binary_fun_sig num_ty num_ty);
   ("/", binary_fun_sig num_ty num_ty);
   ("-", binary_fun_sig num_ty num_ty);

   (">", binary_fun_sig num_ty bool_ty);
   ("<", binary_fun_sig num_ty bool_ty);
   (">=", binary_fun_sig num_ty bool_ty);
   ("<=", binary_fun_sig num_ty bool_ty);

   ("=", binary_fun_sig alpha bool_ty);

   ("#f", bool_ty);
   ("#t", bool_ty);
   ("null", alpha_list);

   ("cons", funtype_of [alpha; alpha_list] alpha_list);
   ("car", unary_fun_sig alpha_list alpha);
   ("cdr", unary_fun_sig alpha_list alpha_list);

   ("ref", unary_fun_sig alpha alpha_ref);
   ("deref", unary_fun_sig alpha_ref alpha);
   ("set", funtype_of [alpha_ref; alpha] alpha);

   ("pair", funtype_of [alpha; beta] (pair_ty alpha beta));
   ("fst", unary_fun_sig (pair_ty alpha beta) alpha);
   ("snd", unary_fun_sig (pair_ty alpha beta) beta);

   ("print", unary_fun_sig alpha unit_ty);
   ("error", unary_fun_sig alpha beta)
  ]

let prim_gamma =
  List.fold_left
    (fun gamma (name, ty) -> tyenv_bind gamma name (generalize_free ty))
    empty_tyenv
    prim_sigs

let nanoml_basis = "
(check-principal-type list1 (forall 'a ('a -> (list 'a))))
(define list1 (x) (cons x '()))

(check-principal-type bind (forall ('a 'b) ('a 'b (list (pair 'a 'b)) -> (list (pair 'a 'b)))))
(define bind (x y alist)
  (if (null? alist)
    (list1 (pair x y))
    (if (= x (fst (car alist)))
      (cons (pair x y) (cdr alist))
      (cons (car alist) (bind x y (cdr alist))))))

(check-principal-type isbound? (forall ('a 'b) ('a (list (pair 'a 'b)) -> bool)))
(define isbound? (x alist)
            (if (null? alist)
              #f
              (if (= x (fst (car alist)))
                #t
                (isbound? x (cdr alist)))))

(check-principal-type find (forall ('a 'b) ('a (list (pair 'a 'b)) -> 'b)))
(define find (x alist)
  (if (null? alist)
    (error 'not-found)
    (if (= x (fst (car alist)))
      (snd (car alist))
      (find x (cdr alist)))))

(check-principal-type caar (forall 'a ((list (list 'a)) -> 'a)))
(check-principal-type cadr (forall 'a ((list 'a) -> 'a)))
(check-principal-type cdar (forall 'a ((list (list 'a)) -> (list 'a))))
(define caar (xs) (car (car xs)))
(define cadr (xs) (car (cdr xs)))
(define cdar (xs) (cdr (car xs)))

(check-principal-type length (forall 'a ((list 'a) -> int)))
(define length (xs)
  (if (null? xs) 0
    (+ 1 (length (cdr xs)))))

(check-principal-type and (bool bool -> bool))
(check-principal-type or (bool bool -> bool))
(check-principal-type not (bool -> bool))
(define and (b c) (if b c b))
(define or (b c) (if b b c))
(define not (b) (if b #f #t))

(check-principal-type o (forall ('a 'b 'c) (('b -> 'c) ('a -> 'b) -> ('a -> 'c))))
(define o (f g) (lambda (x) (f (g x))))

(check-principal-type curry (forall ('a 'b 'c) (('a 'b -> 'c) -> ('a -> ('b -> 'c)))))
(define curry (f) (lambda (x) (lambda (y) (f x y))))
(check-principal-type uncurry (forall ('a 'b 'c) (('a -> ('b -> 'c)) -> ('a 'b -> 'c))))
(define uncurry (f) (lambda (x y) ((f x) y)))

(check-principal-type filter (forall 'a (('a -> bool) (list 'a) -> (list 'a))))
(define filter (p? xs)
    (if (null? xs)
      '()
      (if (p? (car xs))
        (cons (car xs) (filter p? (cdr xs)))
        (filter p? (cdr xs)))))

(check-principal-type map (forall ('a 'b) (('a -> 'b) (list 'a) -> (list 'b))))
(define map (f xs)
    (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))))

(check-principal-type exists? (forall 'a (('a -> bool) (list 'a) -> bool)))
(define exists? (p? xs)
    (if (null? xs)
      #f
      (if (p? (car xs))
        #t
        (exists? p? (cdr xs)))))

(check-principal-type all? (forall 'a (('a -> bool) (list 'a) -> bool)))
(define all? (p? xs)
  (if (null? xs)
    #t
    (if (p? (car xs))
      (all? p? (cdr xs))
      #f)))

(check-principal-type foldr (forall ('a 'b) (('a 'b -> 'b) 'b (list 'a) -> 'b)))
(define foldr (op zero xs)
    (if (null? xs)
      zero
      (op (car xs) (foldr op zero (cdr xs)))))

(check-principal-type foldl (forall ('a 'b) (('a 'b -> 'b) 'b (list 'a) -> 'b)))
(define foldl (op zero xs)
  (if (null? xs)
    zero
    (foldl op (op (car xs) zero) (cdr xs))))

(check-principal-type <= (int int -> bool))
(check-principal-type >= (int int -> bool))
(check-principal-type != (forall 'a ('a 'a -> bool)))
(define <= (x y) (not (> x y)))
(define >= (x y) (not (< x y)))
(define != (x y) (not (= x y)))

(check-principal-type max (int int -> int))
(check-principal-type min (int int -> int))
(check-principal-type mod (int int -> int))
(check-principal-type gcd (int int -> int))
(check-principal-type lcm (int int -> int))
(define max (x y) (if (> x y) x y))
(define min (x y) (if (< x y) x y))
(define mod (m n) (- m (* n (/ m n))))
(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))
(define lcm (m n) (* m (/ n (gcd m n))))

(check-principal-type min* ((list int) -> int))
(check-principal-type max* ((list int) -> int))
(check-principal-type gcd* ((list int) -> int))
(check-principal-type lcm* ((list int) -> int))
(define min* (xs) (foldr min (car xs) (cdr xs)))
(define max* (xs) (foldr max (car xs) (cdr xs)))
(define gcd* (xs) (foldr gcd (car xs) (cdr xs)))
(define lcm* (xs) (foldr lcm (car xs) (cdr xs)))

(check-principal-type unit unit)
(val unit (begin))
(check-principal-type ignore (forall 'a ('a -> unit)))
(define ignore (x) unit)
"
