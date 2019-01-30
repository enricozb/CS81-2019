open Lexing

type loc = 
  { filename   : string;
    start_line : int;
    start_char : int;
    end_line   : int;
    end_char   : int }

let string_of_loc l = 
  "file: " ^
  (if l.filename = "" 
   then "<interactive>; " 
   else "FILE: " ^ l.filename ^ "; ") ^
  Printf.sprintf "line %d, char %d to line %d, char %d" 
    l.start_line l.start_char l.end_line l.end_char

let string_of_loc_short l = 
  let get_loc_string sl sc el ec = 
    match () with
      | _ when sl = el && sc = ec -> Printf.sprintf "%d:%d" sl sc
      | _ when sl = el -> Printf.sprintf "%d:%d-%d" sl sc ec
      | _ -> Printf.sprintf "%d:%d-%d:%d" sl sc el ec
  in
    (if l.filename = "" 
     then "<interactive>: " 
     else l.filename ^ ": ") ^ 
    (get_loc_string l.start_line l.start_char l.end_line l.end_char)

let get_loc fn p s = 
  let ln = p.pos_lnum in
  let sc = p.pos_cnum - p.pos_bol in
  let loc = { filename   = fn;
              start_line = ln;
              end_line   = ln;
              start_char = sc + 1;
              end_char   = sc + String.length s }
  in loc

let span loc1 loc2 = 
  match () with
    | _ when loc1.filename <> loc2.filename ->
      failwith "invalid location span (different filenames)"
    | _ when loc1.start_line > loc2.start_line ->
      failwith "invalid location span (loc1 comes after loc2)"
    | _ when loc1.start_line = loc2.start_line &&
             loc1.start_char > loc2.start_char ->
      failwith "invalid location span (loc1 comes after loc2)"
    | _ -> { filename   = loc1.filename;
             start_line = loc1.start_line;
             start_char = loc1.start_char;
             end_line   = loc2.end_line;
             end_char   = loc2.end_char }

