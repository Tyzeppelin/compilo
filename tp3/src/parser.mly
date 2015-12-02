
%{
    (* Header *)
    open Ast
%}

%token EOL
%token EOF
%token BEGIN END
%token COMMA SEMICOL
%token INT BOOL
%token LEFTASSIGN
%token PLUS
%token INF
%token AND
%token PARL PARR
%token <string> IDENT NUMBER

/* precedence */
%right AND
%right INF
%right PLUS

%start main
%type <Ast.bloc> main

%%

main:
    bloc EOF { $1 } | error { Error }
    ;

bloc:
    BEGIN sdecl SEMICOL sinst END { NodeRoot($2,$4) }
    ;

sdecl:
    decl { [$1] }
    | decl COMMA sdecl { $1::$3 }
    ;

decl:
    typ IDENT { (DeclInt $2) }

    ;

typ:
    INT { fun x -> DeclInt x }
    | BOOL { fun x -> DeclBool x }
    ;

sinst:
    inst { [$1] }
    | inst SEMICOL sinst { $1::$3 }
    ;

inst:
    bloc { NodeBloc $1 }
    | IDENT LEFTASSIGN expr { NodeInst ($1, $3) }
    ;

expr:
    expr PLUS expr { NodePlus ($1, $3) }
    | expr INF expr { NodeInf ($1, $3) }
    | expr AND expr { NodeAnd ($1, $3) }
    | PARL expr PARR { $2 }
    | NUMBER { LeafNum (int_of_string $1) }
    | IDENT { LeafId $1 }
    ;
