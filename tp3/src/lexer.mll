
{
    open Parser
    open Lexing
    exception Eof

    let keyword_table = Hashtbl.create 53

    let _ =
        List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
          [ "begin", BEGIN;
            "end", END;
            "int", INT;
            "bool", BOOL;
            "and", AND;
          ]
}

let l = ['a'-'z''A'-'Z']
let d  = ['0'-'9']
let comment = '%' [^'\n']* '\n'


rule token = parse
    | [' ' '\t']+   { token lexbuf }     (* skip blanks *)
    | '\n'          { Lexing.new_line lexbuf ; token lexbuf }
    | comment       { Lexing.new_line lexbuf ; token lexbuf }
    | '('           { PARL }
    | ')'           { PARR }
    | ';'           { SEMICOL }
    | ','           { COMMA }
    | "<-"          { LEFTASSIGN }
    | '+'           { PLUS }
    | '<'           { INF }
    | l(l|d)* as id { try
                        Hashtbl.find keyword_table id
                      with
                        Not_found -> IDENT id }
    | d+  as num    { NUMBER num }
    | eof           { EOF}
    | _ as c        {Printf.printf "error : %c : unknown token ! \n" c; token lexbuf}
