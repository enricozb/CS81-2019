module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type 'a env = 'a StringMap.t

let empty_env = StringMap.empty

(* Find an element inside of an env, throwing a 'name' error if we can't. *)
let find l env name = try StringMap.find name env
  with Not_found -> Error.name_err l name

(* Find an element inside of an env, letting a Not_found error propagate
 * upwards instead. *)
let raw_find env name = StringMap.find name env

(* Find a single name and value (both curried and uncurried) *)
let bind env name v = StringMap.add name v env
let bind_pair env (name, v) = bind env name v

(* TODO check names.length == vs.length ! *)
let bind_list env names vs = List.fold_left2 bind env names vs
let bind_pairs env pairs = List.fold_left bind_pair env pairs
