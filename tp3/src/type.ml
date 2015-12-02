open Lexing
open Ast

let rec ast_to_string ast lexbuf =
    (* fonctions servant à afficher la ligne et la position
    de l'erreur *)
    let pos = lexeme_start_p lexbuf
    in
    let line = (string_of_int (pos.pos_lnum))
    in
    (* cnum est le nombre de caractères entre le début du buffer et la position courante.
    bol est le nombre de caractère entre le début du buffer et le debut de la ligne courante.
    On fait -1 pour commencer à 0 *)
    let colBegin = (string_of_int (pos.pos_cnum - pos.pos_bol - 1))
    in
    (* fonctions permettant de traduire l'arbre en string *)
    let var_to_string var =
        match var with
        | DeclInt var -> "variable " ^ var ^ " : int\n"
        | DeclBool var -> "variable " ^ var ^ " : bool\n" in
        let rec decl_to_string decl =
            match decl with
            | [] -> ""
            | hd :: tl -> (var_to_string hd) ^ (decl_to_string tl)
        in
        let rec expr_to_string expression =
            match expression with
            | NodePlus (expr1, expr2) -> (expr_to_string expr1) ^ " + " ^ (expr_to_string expr2)
            | NodeInf (expr1, expr2) -> (expr_to_string expr1) ^ " < " ^ (expr_to_string expr2)
            | NodeAnd (expr1, expr2) -> (expr_to_string expr1) ^ " and " ^ (expr_to_string expr2)
            | LeafNum exp -> string_of_int exp
            | LeafId exp -> exp
        in
        let affect_to_string affect =
            match affect with
            | NodeBloc a -> (ast_to_string a lexbuf)
            | NodeInst (var, expr) -> "var " ^ var ^ " = " ^ (expr_to_string expr) ^ "\n"
        in
        let rec instruction_to_string inst =
            match inst with
            | [] -> ""
            | hd :: tl -> (affect_to_string hd) ^ (instruction_to_string tl) 
        in
    match ast with
    | NodeRoot (decl, inst) -> "Déclarations :\n" ^ (decl_to_string decl) ^
        "\nInstrcutions :\n"^(instruction_to_string inst)
    | Error -> "Erreur ligne " ^ line ^ ", colonne " ^ colBegin ^ "\n"
