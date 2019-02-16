(* env.mli : environments *)

module StringMap : Map.S with type key = string
module StringSet : Set.S with type elt = string

type 'a env = 'a StringMap.t
val empty_env : 'a StringMap.t
val find : Loc.loc -> 'a StringMap.t -> StringMap.key -> 'a
val raw_find : 'a StringMap.t -> StringMap.key -> 'a
val bind : 'a StringMap.t -> StringMap.key -> 'a -> 'a StringMap.t
val bind_pair : 'a StringMap.t -> StringMap.key * 'a -> 'a StringMap.t
val bind_list :
  'a StringMap.t -> StringMap.key list -> 'a list -> 'a StringMap.t
val bind_pairs :
  'a StringMap.t -> (StringMap.key * 'a) list -> 'a StringMap.t
