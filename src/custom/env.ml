module StringMap = Map.Make(String)
module Dom = Set.Make(String)

type 'a env = 'a StringMap.t

let empty = StringMap.empty

let dom env =
  StringMap.fold (fun key _ set -> Dom.add key set) env Dom.empty

let mem id env = StringMap.mem id env
let get id env = StringMap.find id env
let lookup l id env =
  try
    StringMap.find id env
  with
    Not_found -> Error.name_error l id

let bind id value env = StringMap.add id value env

let rec bind_many ids values env =
  match (ids, values) with
  | ([], []) -> env
  | (id :: ids, value :: values) ->
      bind_many ids values (bind id value env)
  | _ -> failwith "Env.bind_many different length lists"

let bind_pairs bindings env =
  let (ids, values) = List.split bindings in
  bind_many ids values env

