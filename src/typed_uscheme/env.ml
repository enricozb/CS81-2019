module StringMap = Map.Make(String)

(* An environment will be an immutable mapping of locals to locations, so that
 * we can save it at any point to create a closure, plus a mutable mapping of
 * globals to values, because globals are effectively dynamically scoped in
 * Scheme. *)

type env = { locals: (value ref) StringMap.t;
             globals: (string, value) Hashtbl.t }
and value =
  | PrimFuncVal of (value list -> Loc.loc -> value)
  (* Note that user-defined functions capture an environment. *)
  | UserFuncVal of Syntax.id list * Syntax.expr * env
  | IntVal of int
  | BoolVal of bool
  | PairVal of value * value
  (* Empty list. *)
  | NilVal
  (* Unit value--value of e.g. `set` and `while`. *)
  | UnitVal
  (* For temporarily putting in environments to allow recursion.
   * Any attempt to actually evaluate this will be an error. *)
  | Unspecified of Syntax.id

let rec string_of_val = function
  | UserFuncVal _
  | PrimFuncVal _ -> "<function>"
  | IntVal i -> string_of_int i
  | BoolVal true -> "#t"
  | BoolVal false -> "#f"
  | PairVal (a, b) ->
    "'(" ^ string_of_pair a b ^ ")"
  | NilVal -> "nil"
  | UnitVal -> "#u"
  | Unspecified s -> "<placeholder for " ^ s ^ ">"
and string_of_pair a =
  let a = string_of_val a in function
    | NilVal -> a
    | PairVal (b, c) -> a ^ " " ^ string_of_pair b c
    | other -> a ^ " . " ^ string_of_val other

let truthy l = function
  | BoolVal false -> false
  | Unspecified s -> Error.name_err l s
  (* This is actually R5RS. *)
  | _ -> true

let lookup l env name = try !(StringMap.find name env.locals)
  with Not_found -> try Hashtbl.find env.globals name
    with Not_found -> Error.name_err l name

let set l env name v =
  try StringMap.find name env.locals := v
  with Not_found -> if Hashtbl.mem env.globals name then
      Hashtbl.replace env.globals name v
    else
      Error.name_err l name

let define env name v = Hashtbl.replace env.globals name v

let bind_local env name v =
  {env with locals=StringMap.add name (ref v) env.locals}

let hashtbl_of_alist alist buckets =
  let tbl = Hashtbl.create buckets in
  List.iter (fun (k, v) -> Hashtbl.replace tbl k v) alist;
  tbl

let make_env alist =
  {
    locals = StringMap.empty;
    globals = hashtbl_of_alist alist 1024
  }
