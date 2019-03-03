type error =
  | NameError of string
  | TypeError of string

exception MythError of Loc.loc * error

let string_of_error l = function
  | NameError s -> (Loc.string_of_loc_short l) ^ "; NameError: '" ^ s ^ "'"
  | TypeError s -> (Loc.string_of_loc_short l) ^ "; TypeError: '" ^ s ^ "'"

let print_error l error = Printf.printf "%s\n" (string_of_error l error)

(* Raise Error functions *)
let error l err = raise (MythError (l, err))
let name_error l id = error l (NameError id)
let type_error l id = error l (TypeError id)
let call_len_error l ~fun_ty ~expected ~provided =
  error l (TypeError (fun_ty ^ " takes " ^ (string_of_int expected) ^
           " positional arguments but " ^ (string_of_int provided) ^
           " were given"))
let call_error l invalid_fun_ty =
  error l (TypeError ("'" ^ invalid_fun_ty ^ "' is not callable"))

