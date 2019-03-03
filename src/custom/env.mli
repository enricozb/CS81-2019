(* env.mli : environments, ty_env & val_env *)

module StringMap : Map.S with type key = string
module Dom : Set.S with type elt = string

type 'a env = 'a StringMap.t

val empty : 'a env

val dom : 'a env -> Dom.t
val mem : StringMap.key -> 'a env -> bool
val get : StringMap.key -> 'a env -> 'a
val lookup : Loc.loc -> StringMap.key -> 'a env -> 'a
val bind : StringMap.key -> 'a -> 'a env -> 'a env
val bind_many : StringMap.key list -> 'a list -> 'a env -> 'a env

