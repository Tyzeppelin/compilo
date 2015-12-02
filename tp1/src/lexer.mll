{
  open Ulex (* Ulex contains the type definition of lexical units *)
}

let letter = ['A'-'Z''a'-'z']
let number = ['0'-'9']
let eol = ['\n']
let blank = [' ' '\t']

rule token = parse (* TODO *)
   (*| ['0']   { UL_UNIT}*)
    | blank     { token lexbuf }
    | eol       { token lexbuf }
(*    | '/'+'*' ( '/'+'*' | [^'*''/'] | '*'+[^'/'] | '/'+[^'*'] )*? '*'+'/'  { token lexbuf }*)
    (*Expression régulière de @AurélienFontaine qui fonctionne un peu mieux que la mienne*)
    | '/'+'*' ('*'+[^'*''/'] | [^'*'])* '*'+'/' { token lexbuf }
    | "or"      { UL_OR }
    | "and"     { UL_AND }
    | ">"       { UL_SUP }
    | "<"       { UL_INF }
    | "<>"      { UL_DIFF }
    | "="       { UL_EQUAL }
    | "("       { UL_PAROPEN }
    | ")"       { UL_PARCLOSE }
    | letter (letter |  number)* as lxm { UL_IDENT(lxm)}
    | eof       { UL_EOF }
    | _ as c  {Printf.printf "error : %c \n" c; token lexbuf}
