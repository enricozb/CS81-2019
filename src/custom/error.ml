type error =
  | RuntimeError of string
  | NameError of string
  | BindError of string
  | TypeError of string
  | SyntaxError of string

exception MythError of Loc.loc * error

let string_of_error l = function
  | RuntimeError s -> (Loc.string_of_loc_short l) ^ "; RuntimeError: '" ^ s ^ "'"
  | BindError s -> (Loc.string_of_loc_short l) ^ "; BindError: '" ^ s ^ "'"
  | NameError s -> (Loc.string_of_loc_short l) ^ "; NameError: '" ^ s ^ "'"
  | TypeError s -> (Loc.string_of_loc_short l) ^ "; TypeError: '" ^ s ^ "'"
  | SyntaxError "" ->
      (Loc.string_of_loc_short l) ^ "; SyntaxError"
  | SyntaxError s ->
      (Loc.string_of_loc_short l) ^ "; SyntaxError: '" ^ s ^ "'"

let print_error l error = Printf.printf "%s\n" (string_of_error l error)

(* Raise Error functions *)
let error l err = raise (MythError (l, err))

let runtime_error l s = error l (RuntimeError s)


let name_error l id = error l (NameError id)

let type_error l id = error l (TypeError id)

let syntax_error l id = error l (SyntaxError id)



let type_mismatch_error l ~expected ~provided =
  error l (TypeError ("Expected " ^ expected ^ " but got " ^ provided))

let unify_error l ty1 ty2 =
  error l (TypeError ("Cannot unify types " ^ ty1 ^ " and " ^ ty2 ^ "."))

let missing_field l field =
  error l (TypeError ("Field '" ^ field ^ "' is missing"))

let call_len_error l ~fun_ty ~expected ~provided =
  error l (TypeError (fun_ty ^ " takes " ^ (string_of_int expected) ^
           " positional arguments but " ^ (string_of_int provided) ^
           " were given"))

let call_error l invalid_fun_ty =
  error l (TypeError ("'" ^ invalid_fun_ty ^ "' is not callable"))

let unreachable_code_error l flow =
  syntax_error l ("Code after '" ^ flow ^ "' is unreachable")

let return_outside_def l =
  syntax_error l "'return' can only occur inside a function"

let flow_outside_loop l =
  syntax_error l "'break' and 'continue' can only occur inside a loop"

let bind_error l id =
  error l (BindError ("cannot assign to '" ^ id ^ "' as it is not mutable"))

