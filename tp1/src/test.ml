open Ulex

(** scanner : lexbuf -> token list
    scanner lexbuf constructs the list of token read from the argument lexbuf
    (ends when reading UL_EOF )
*)
let scanner lexbuf =

  let rec scanner_rec n l =
    try
      match Lexer.token lexbuf with
      | UL_EOF as tk ->  (tk::l)
      |   tk   -> scanner_rec (n+1) (tk :: l)
  with x ->
    begin
    Printf.printf "Warning : exception %s was raised after reading %i tokens\n"
      (Printexc.to_string x) n;
      l
    end
  in
    List.rev (scanner_rec 0 [])



let ex1 = "jojo and jaja"

(*let _ =
  (** 1. Construct a lex buffer from the standard input channel *)
  let lexbuf = Lexing.from_channel stdin in

  (** 2. Construct the list of tokens *)
  let tokens = scanner lexbuf in

  (** 3. Print the tokens *)
  List.iter (fun tk -> Printf.fprintf stdout "Token: %a\n" Ulex.print_token tk) tokens ;
  Printf.printf "DONE\n"
*)

(*Testing things*)
let test = [

    (*basic Test*)
("toto and titi tutu or tata",
    [UL_IDENT "toto";UL_AND; UL_IDENT "titi"; UL_IDENT "tutu";UL_OR ; UL_IDENT "tata"; UL_EOF]);


("toto and or titi = rata",
    [UL_IDENT "toto";UL_AND;UL_OR;UL_IDENT "titi";UL_EQUAL;UL_IDENT "rata";UL_EOF]);

("tata and or <> = )( /*aa*/ > a2 < az2az2",
    [UL_IDENT "tata";UL_AND;UL_OR;UL_DIFF;UL_EQUAL;UL_PARCLOSE;UL_PAROPEN;UL_SUP;UL_IDENT "a2";UL_INF;UL_IDENT "az2az2";UL_EOF]);

    (*Some indentation tests, OK *)
("toto and titi/*/*testst ***/",
    [UL_IDENT "toto";UL_AND;UL_IDENT "titi";UL_EOF]);

("ok /* ignor */ ok /* ignore */ fin",
    [UL_IDENT "ok"; UL_IDENT "ok"; UL_IDENT "fin"; UL_EOF]);

("er /***/  or ere /*/*/ and (ee =/*/**/aa)",
    [UL_IDENT "er"; UL_OR; UL_IDENT "ere"; UL_AND; UL_PAROPEN; UL_IDENT "ee"; UL_EQUAL; UL_IDENT "aa"; UL_PARCLOSE; UL_EOF]);
    ]

(* Compare the expressions and their suppose result*)
let analyse exp res =
    (** Construct the list of tokens *)
    let tokens = scanner exp
        in
    if res=tokens
        then Printf.printf "OK\n"
        else Printf.printf "FAIL\n"


(* Rocket Man - David Bowie *)
(*Launching....*)
let _ =
    List.iter
            (fun a-> let lexbuf = Lexing.from_string (fst a)
                in analyse lexbuf (snd a))
            test

