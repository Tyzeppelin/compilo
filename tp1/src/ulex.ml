(** [token] is the type of the different lexical units. *)
type token =  UL_OR
             | UL_AND
             | UL_SUP
             | UL_INF
             | UL_EQUAL
             | UL_DIFF
             | UL_PAROPEN
             | UL_PARCLOSE
             | UL_IDENT of string
             | UL_EOF

(** [is_eof] : token  -> bool
    is_eof tk returns true if the lexical unit represents the end_of file.
*)
let is_eof = function
  | UL_EOF -> true
  | _      -> false

(** [print_token] : out_channel -> token -> unit
    print_token o tk prints on the output channel o the textual representation of the token tk *)
let print_token o = function
  | UL_OR       -> Printf.fprintf o "UL_OR"
  | UL_AND      -> Printf.fprintf o "UL_AND"
  | UL_SUP      -> Printf.fprintf o "UL_SUP"
  | UL_INF      -> Printf.fprintf o "UL_INF"
  | UL_DIFF     -> Printf.fprintf o "UL_DIFF"
  | UL_EQUAL    -> Printf.fprintf o "UL_EQUAL"
  | UL_PAROPEN  -> Printf.fprintf o "UL_PAROPEN"
  | UL_PARCLOSE -> Printf.fprintf o "UL_PARCLOSE"
  | UL_IDENT(s) -> Printf.fprintf o "UL_IDENT %s" s
  | UL_EOF      -> Printf.fprintf o "eof"

