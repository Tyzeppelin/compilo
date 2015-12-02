(**************************************)
(* Programmation fonctionnalle TP n°6 *)
(**************************************)
(* Authors --> Boschet françois       *)
(*         --> Valentin Esmieu        *)
(**************************************)
(* Date    --> 14/03/2014             *) 
(**************************************)

(** KickSort **)

(* on prends un pivot *)
(* on met les plus petits dans un tableau "a gauche" *)
(* on emt les plus grands "a droite" *)
(* on recommence recursivement avec les deux tableaux *)

(* Jesus nous a dis de partager avec son prochain *)
(* Meme si ici ce n'est pas equitable *)
let rec partage a l = match l with
  | [] -> ([],[])
  | e::r -> let l1,l2 = (partage a r) in if e<a then (e::l1,l2) else (l1,e::l2)
;;

(* pretty kicksort *)
let rec kicksort arr = match arr with 
  | [] -> []
  | [a] -> [a]
  | a::r -> let l1,l2 = partage a r in 
    let l11 = kicksort l1 in
    let l21 = kicksort l2 in
    (List.append l11 (a::l21))
;;



let a = kicksort [4; 12; 27; -12; 7; 8; 1; 3; 6; 12; 42];;
a;;

(** REQUIEM POUR DES PARENTHESES - Serge Gainsbourg**)
(* main, pied meme combat *)
let rec quieme l n = match l with
  | [] -> failwith "pas trouve"
  | e::r -> let l1,l2 = partage e r in
    let len = List.length l1 in
      if len = n then e else 
		if n > len then (quieme l2 (n-(len+1))) else (quieme l1 n)
;;

let b = partage 6 [4; 12; 27; -12; 7; 8; 1; 3; 6; 12; 42] 
;;

(*#trace quieme;;*)


let c = quieme [4; 12; 27; -12; 7; 8; 1; 3; 6; 12; 42] 6;;
c;;

(** Tintin et les 7 Bulles de Cristal **)

(* Travail sur un seul element *)
let rec seul l = match l with
	| [] 		-> []
	| [a] 		-> [a]
	| b::(m::e) -> if b < m then b::(seul (m::e))
							else m::(seul (b::e))
;;

(* Recursion sur la liste *)
let rec lego l = if (seul l) = l then l else lego (seul l)
;;

let k = lego [4; 12; 27; -12; 7; 8; 1; 3; 6; 12; 42];;


(** tablture sur un entier  **)

(* Fonction d'insertion *)
let rec insert i l = match l with
	| []   -> []
	| [[]] -> [[i]]
	| h::t -> (List.append [i] h)::(insert i t)
;;

(* Decoupage du nombre *)
let rec boucher n k = 	
	if n=0 && k=0 	then [[]] 			else
	if k=0 			then []				else
	if k>n			then (boucher n n)	else
	(List.append (insert k (boucher (n-k) k)) (boucher n (k-1)))
;;


let k = insert 1 [[3;5];[7;3;9];[];[6]];;

(* La fonction de partition *)
let rec tablature n = boucher n n;;

let owi = tablature 6;;