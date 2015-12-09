%{
  open Ast
%}

%token <int> INT
%token <string> IDENT
%token TRUE FALSE
%token LET REC IN
%token FUNCTION ARROW ALTERNATIVE 
%token IF THEN ELSE
%token ADD SUB MULT LESS
%token FST SND
%token EQUAL
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACKET RIGHT_BRACKET CONS
%token COMMA
%token END_OF_EXPRESSION
%token EOF
%token COLON
%token TBOOL TINT TLIST

%nonassoc NO_ALTERNATIVE
%nonassoc ALTERNATIVE IN
%left LESS EQUAL
%right CONS
%left ADD SUB
%left MULT
%nonassoc FST SND ELSE

%start main
%type <Ast.ml_ast * StrSet.t > main
%%

main:
 | EOF { Printf.printf "\nbye"; exit 0 }
 | LET IDENT EQUAL expr END_OF_EXPRESSION { Ml_definition($2, fst $4), (*TODO*) StrSet.empty }
 | LET REC IDENT EQUAL expr END_OF_EXPRESSION { Ml_definitionrec($3, failwith "let rec type expected", fst $5) , (*TODO*) StrSet.empty }
 | expr END_OF_EXPRESSION { Ml_expr (fst $1) , (*TODO*) StrSet.empty}
 | error {
    let bol = (Parsing.symbol_start_pos ()).Lexing.pos_bol in
    failwith ("parsing: line " ^ 
		 (string_of_int ((Parsing.symbol_start_pos ()).Lexing.pos_lnum)) ^ 
		 " between character " ^
		 (string_of_int (Parsing.symbol_start () - bol)) ^
		 " and " ^
		 (string_of_int ((Parsing.symbol_end ()) + 1 - bol)))
 }

expr:
 | simple_expr { $1 }
 | LEFT_PAREN expr RIGHT_PAREN { $2 }
 | expr CONS expr { Ml_cons(fst $1, fst $3) , (*TODO*) StrSet.empty }
 | LEFT_PAREN expr COMMA expr RIGHT_PAREN { Ml_pair(fst $2, fst $4) , (*TODO*) StrSet.empty }
 | FST expr { Ml_unop(Ml_fst, fst $2) , (*TODO*) StrSet.empty }
 | SND expr { Ml_unop(Ml_snd, fst $2) , (*TODO*) StrSet.empty }
 | expr ADD expr { Ml_binop(Ml_add, fst $1, fst $3) , (*TODO*) StrSet.empty }
 | expr MULT expr { Ml_binop(Ml_mult, fst $1, fst $3) , (*TODO*) StrSet.empty }
 | expr SUB expr { Ml_binop(Ml_sub, fst $1, fst $3),  (*TODO*) StrSet.empty }
 | expr LESS expr { Ml_binop(Ml_less, fst $1, fst $3), (*TODO*) StrSet.empty }
 | expr EQUAL expr { Ml_binop(Ml_eq, fst $1, fst $3), (*TODO*) StrSet.empty }
 | IF expr THEN expr ELSE expr { Ml_if(fst $2, fst $4, fst $6), (*TODO*) StrSet.empty }
 | FUNCTION pattern_expr_list { Ml_fun (fst $2) , (*TODO*) StrSet.empty }
 | application { List.fold_left (fun res a -> Ml_app(res, a)) (List.hd (fst $1)) (List.tl (fst $1)) , (*TODO*) StrSet.empty    }
 | LET IDENT  EQUAL expr IN expr { Ml_let($2, fst $4, fst $6) , (*TODO*) StrSet.empty }
 | LET REC IDENT  EQUAL expr IN expr { Ml_letrec($3, failwith "let rec type expected", fst $5, fst $7) , (*TODO*) StrSet.empty }

simple_expr:
 | INT { Ml_int $1, (*TODO*) StrSet.empty }
 | bool { Ml_bool $1, (*TODO*) StrSet.empty }
 | LEFT_BRACKET RIGHT_BRACKET  { Ml_nil (failwith "[] : type expected"), (*TODO*) StrSet.empty }
 | IDENT { Ml_var $1 ,(*TODO*) StrSet.empty }

bool:
 | FALSE { false }
 | TRUE  { true }

pattern:
 | IDENT { Ml_pattern_var ($1,failwith "pattern variable: type expected") , (*TODO*) StrSet.empty} 
 | INT   { Ml_pattern_int $1 , (*TODO*) StrSet.empty }
 | bool  { Ml_pattern_bool $1 , (*TODO*) StrSet.empty }
 | LEFT_PAREN pattern COMMA pattern RIGHT_PAREN 
   {Ml_pattern_pair(fst $2, fst $4), (*TODO*) StrSet.empty  }
 | LEFT_BRACKET RIGHT_BRACKET { Ml_pattern_nil (failwith "pattern []: type expected") , (*TODO*) StrSet.empty }
 | pattern CONS pattern { Ml_pattern_cons(fst $1, fst $3), (*TODO*) StrSet.empty }

pattern_expr_list:
 | pattern ARROW expr pattern_expr_list_next { (fst $1, fst $3) :: fst $4 , (*TODO*) StrSet.empty}

pattern_expr_list_next:
 | ALTERNATIVE pattern ARROW expr pattern_expr_list_next { (fst $2, fst $4) :: fst $5, (*TODO*) StrSet.empty }
 | %prec NO_ALTERNATIVE { [] , (*TODO*) StrSet.empty }

application:
 | simple_expr_or_parenthesized_expr simple_expr_or_parenthesized_expr application_next { fst $1 :: fst $2 :: fst $3, (*TODO*) StrSet.empty }

simple_expr_or_parenthesized_expr:
 | simple_expr { $1 }
 | LEFT_PAREN expr RIGHT_PAREN { $2 }

application_next:
 | simple_expr_or_parenthesized_expr application_next { fst $1 :: fst $2 , (*TODO*) StrSet.empty }
 | { [] , (*TODO*) StrSet.empty }


