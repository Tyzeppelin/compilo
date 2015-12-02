{
  open Parser
  let keyword_table = Hashtbl.create 53
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [
        "let", LET;
        "rec", REC;
        "in", IN;

        "function", FUNCTION;

        "if" , IF;
        "then", THEN;
        "else", ELSE;

        "true", TRUE;
        "false", FALSE;

        "fst", FST;
        "snd", SND;
      ]
}

let space = [' ' '\t']

let letter = ['A'-'Z' 'a'-'z' '_']

let digit = ['0'-'9']

rule get_token = parse
  | "//" [^'\n']* '\n'? { get_token lexbuf }
  | "/*" ([^'*']|('*'+[^'*''/']))* '*'+ '/' { get_token lexbuf}
  | ['\n' '\r'] { Lexing.new_line lexbuf; get_token lexbuf }
  | ','  { COMMA }
  | '('  { LEFT_PAREN }
  | ')'  { RIGHT_PAREN }
  (*arithmetic*)
  | '+'  { ADD }
  | '-'  { SUB }
  | '*'  { MUL }
  (*| '/'  { DIV }*)
  | '='  { EQUAL }
  | "<"  { INF }
  (*
   *| ">"  { SUP }
   *| "<=" { INFEQ }
   *| ">=" { SUPEQ }
   *)
  (*lists*)
  | '[' { LEFT_BRACKET }
  | ']' { RIGHT_BRACKET }
  | "::" { CONS }
  (*function*)
  | '|' { ALTERNATIVE }
  | "->"{ ARROW }
  (*==*)
  | ";;" { END_OF_EXPRESSION }
  | space { get_token lexbuf }
  | digit+ as v { INT (int_of_string v) }
  | letter(letter|digit)* as id {
    try
        Hashtbl.find keyword_table id
    with Not_found -> IDENT id
  }
  | eof { EOF }
