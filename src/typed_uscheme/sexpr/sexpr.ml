open Loc

type expr =
  | Int  of loc * int
  | Id   of loc * string
  | List of loc * expr list

let loc_of_expr = function
  | Int  (l, _) -> l
  | Id   (l, _) -> l
  | List (l, _) -> l

let string_of_expr sx =
  let spaces n = String.make n ' ' in
  let rec iter sx indent =
    spaces indent ^
    (match sx with
      | Int  (_, i)     -> "INT[" ^ (string_of_int i) ^ "]"
      | Id   (_, i)     -> "ID[" ^ i ^ "]"
      | List (_, slist) ->
        "LIST[\n"
        ^ iter_list slist (indent + 2)
        ^ spaces indent ^ "]")
    ^ "\n"
  and iter_list slist indent =
    begin
      match slist with
        | [] -> ""
        | [s] -> iter s indent
        | h :: t -> iter h indent ^ iter_list t indent
    end
  in
    iter sx 0

let string_of_expr_loc sx =
  let sol loc = "{ " ^ string_of_loc_short loc ^ " }" in
  let spaces n = String.make n ' ' in
  let rec iter sx indent =
    spaces indent ^
    (match sx with
      | Int  (loc, i)     -> "INT[" ^ (string_of_int i) ^ "]  " ^ sol loc
      | Id   (loc, i)     -> "ID[" ^ i ^ "]  " ^ sol loc
      | List (loc, slist) ->
        "LIST[\n"
        ^ iter_list slist (indent + 2)
        ^ spaces indent ^ "]  " ^ sol loc)
    ^ "\n"
  and iter_list slist indent =
    begin
      match slist with
        | [] -> ""
        | [s] -> iter s indent
        | h :: t -> iter h indent ^ iter_list t indent
    end
  in
    iter sx 0
