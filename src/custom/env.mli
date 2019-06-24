(* env.mli : environments, ty_env & val_env *)

module StringMap : Map.S with type key = string
module Dom : Set.S with type elt = string

type 'a env = 'a StringMap.t

val empty : 'a env

val dom : 'a env -> Dom.t
val mem : StringMap.key -> 'a env -> bool
val get : StringMap.key -> 'a env -> 'a
val get_opt : StringMap.key -> 'a env -> 'a option
val lookup : Loc.loc -> StringMap.key -> 'a env -> 'a
val bind : StringMap.key -> 'a -> 'a env -> 'a env
val bind_many : StringMap.key list -> 'a list -> 'a env -> 'a env
val bind_pairs : (StringMap.key * 'a) list -> 'a env -> 'a env
val map : ('a -> 'b) -> 'a env -> 'b env
val right_union : 'a env -> 'a env -> 'a env
val bindings : 'a env -> (StringMap.key * 'a) list
