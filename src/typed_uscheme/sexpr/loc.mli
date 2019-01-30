(* Type of locations in a source code file.
 * Locations are a range of positions between
 * (start_line, start_char) and (end_line, end_char) inclusive.
 * Lines and characters use 1-indexing, as do most editors.
 * The filename field is blank for interactive use. *)
type loc = 
  { filename   : string;
    start_line : int;
    start_char : int;
    end_line   : int;
    end_char   : int }

(* Pretty-print a location. *)
val string_of_loc : loc -> string

(* Pretty-print a location; short form. *)
val string_of_loc_short : loc -> string

(* Get a loc from a lexeme.  The first argument is the filename. *)
val get_loc : string -> Lexing.position -> string -> loc

(* Get a loc from two locs. 
 * The loc has the same filename as the two input locs,
 * The (start_line, start_char) from the first input loc,
 * and the (end_line, end_char) from the second input loc.
 * Raise an exception if the filenames are different or
 * if the first loc doesn't come strictly before the second. *)
val span : loc -> loc -> loc

