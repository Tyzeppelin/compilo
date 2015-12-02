(**************************************)
(* Programmation fonctionnalle TP n°6 *)
(**************************************)
(* Authors --> Boschet françois       *)
(*         --> Valentin Esmieu        *)
(**************************************)
(* Date    --> 14/03/2014             *) 
(**************************************)


(* on prends un pivot *)
(* on met les plus petits dans un tableau "a gauche" *)
(* on emt les plus grands "a droite" *)
(* on recommence recursivement avec les deux tableaux *)


let rec quicksort arr = match arr with 
  | [] -> []
  | [a] -> [a]
  | a::r -> let l1,l2 = partage a r in 
    let l11 = quicksort l1 in
    let l21 = quicksort l2 in
    (List.append l11 (a::l21))
;;


let rec partage a l = match l with
  | [] -> ([],[])
  | e::r -> let l1,l2 = (partage a r) in if e<a then (e::l1,l2) else (l1,e::l2)
;;


let a = quicksort [4; 12; 27; -12; 7; 8; 1; 3; 6; 12; 42];;
a;;

(* REQUIEM POUR DES PARENTHESES - Serge Gainsbourg*)
let rec quieme l n = match l with
  | [] -> failwith "pas trouve"
  | e::r -> let l1,l2 = partage e r in
    let len = List.length l1 in
      if len = n then e else 
	if n > len then (quieme l2 (n-(len+1))) else (quieme l1 n)
;;

let b = partage 6 [4; 12; 27; -12; 7; 8; 1; 3; 6; 12; 42] 
;;

#trace quieme;;


let c = quieme [4; 12; 27; -12; 7; 8; 1; 3; 6; 12; 42] 6;;
c;;


