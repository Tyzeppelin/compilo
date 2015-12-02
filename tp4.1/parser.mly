%{
  open Ast
%}

%token <int> INT
%token <string> IDENT
%token LET REC IN
%token FUNCTION ARROW ALTERNATIVE
%token IF THEN ELSE
%token TRUE FALSE
%token FST SND
%token ADD SUB MUL DIV
%token EQUAL INF
%token LEFT_BRACKET RIGHT_BRACKET CONS
%token ALTERNATIVE ARROW
%token LEFT_PAREN RIGHT_PAREN
%token COMMA
%token EOF END_OF_EXPRESSION

%nonassoc NO_ALTERNATIVE
%nonassoc ALTERNATIVE IN
%left INF EQUAL
%right CONS
%left ADD SUB
%left MUL
%nonassoc FST SND ELSE

%start main
%type <Ast.ml_expr> main
%%

main:
 | EOF { Printf.printf "\nbye\n"; exit 0 }
 | expr END_OF_EXPRESSION { $1 }
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
| simple_expr_or_parenthesized_expr { $1 }
| FST expr { Ml_unop(Ml_fst, $2) }
| SND expr { Ml_unop(Ml_snd, $2) }
| expr ADD expr { Ml_binop(Ml_add, $1, $3) }
| expr SUB expr { Ml_binop(Ml_sub, $1, $3) }
| expr MUL expr { Ml_binop(Ml_mul, $1, $3) }
| expr INF expr { Ml_binop(Ml_inf, $1, $3) }
| expr EQUAL expr { Ml_binop(Ml_eq, $1, $3) }
| IF expr THEN expr ELSE expr { Ml_if($2, $4, $6) }
| FUNCTION fct { Ml_fun $2 }
| application { List.fold_left (fun res a -> Ml_app(res, a)) (List.hd $1) (List.tl $1) }
| LET IDENT EQUAL expr IN expr { Ml_let($2, $4, $6) }
| LET REC IDENT EQUAL expr IN expr { Ml_letrec($3, $5, $7) }

simple_expr:
| INT { Ml_int $1 }
| bool { Ml_bool $1 }
| IDENT { Ml_var $1 }
| LEFT_BRACKET RIGHT_BRACKET { Ml_nil }

bool:
 | FALSE { false }
 | TRUE { true }

application:
 | simple_expr_or_parenthesized_expr simple_expr_or_parenthesized_expr application_next { $1 :: $2 :: $3 }

application_next:
 | simple_expr_or_parenthesized_expr application_next { $1 :: $2 }
 |  { [] }

fct:
 | pattern ARROW expr fct_next { ($1, $3) :: $4 }

fct_next:
 | ALTERNATIVE pattern ARROW expr fct_next { ($2, $4) :: $5 }
 | %prec NO_ALTERNATIVE { [] }

pattern:
 | INT { Ml_pattern_int $1 }
 | IDENT { Ml_pattern_var $1 }
 | bool { Ml_pattern_bool $1 }
 | LEFT_PAREN pattern COMMA pattern RIGHT_PAREN { Ml_pattern_pair($2, $4) }
 | LEFT_BRACKET RIGHT_BRACKET { Ml_pattern_nil }
 | pattern CONS pattern { Ml_pattern_cons($1, $3) }


simple_expr_or_parenthesized_expr:
 | simple_expr { $1 }
 | LEFT_PAREN expr COMMA expr RIGHT_PAREN { Ml_pair($2, $4) }
 | LEFT_PAREN expr RIGHT_PAREN { $2 }


