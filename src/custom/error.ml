type error =
  | NameError of string

exception MythError of Loc.loc * error

let string_of_error l = function
  | NameError s -> (Loc.string_of_loc_short l) ^ "; Unknown name '" ^ s ^ "'"

let print_error l error = Printf.printf "%s\n" (string_of_error l error)

(* Raise Error functions *)
let error l err = raise (MythError (l, err))
let name_error l id = error l (NameError id)



