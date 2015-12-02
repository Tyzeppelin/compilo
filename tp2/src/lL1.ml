open Grammar

open Grammar
open Equation

(** [is_vn vn v] returns whether v is the non-terminal vn *)
let is_vn : vn -> v -> bool =
 fun vn v ->
    match v with
    | Vt x -> false
    | Vn y -> vn_equal y vn

(** Accessibility *)
let accv : vn -> var =
 fun vn -> ("Acc",vn)

let acc_vn : grammar -> vn -> bexpr =
 fun g vn ->
  let l = gather_sprod_using_vn g vn in
  if vn_equal vn (axiom g)
  then Bool true
  else
   List.fold_left (fun e sp -> Or(BVar(accv sp.ntl),e)) (Bool false) l

let acc_sys : grammar -> equation list =
 fun g -> make_bool_system g "Acc" acc_vn

let acc : grammar -> vn -> bool =
 fun g ->
  let sol = solve (acc_sys g) in
  fun vn -> fst sol ("Acc",vn)

let check_acc : grammar -> bexpr =
fun g ->  VNSet.fold (fun vn c -> And(BVar(accv vn),c)) (nterminals g) (Bool true)

(** Generative *)
let genv : vn -> var =
 fun vn -> ("Gen",vn)

let rec gen_prod : v list -> bexpr =
 fun l ->
  match l with
      | [] -> Bool true
   | Vt _ :: l -> gen_prod l
   | Vn v :: l -> And(BVar (genv v),gen_prod l)

let gen_vn : grammar -> vn -> bexpr =
 fun g vn ->
 let ls = production g vn in
 List.fold_left (fun acc vs -> Or(acc,gen_prod vs)) (Bool false) ls

let gen_sys : grammar -> equation list =
 fun g -> make_bool_system g "Gen" gen_vn

let gen : grammar -> vn -> bool =
 fun g  ->
  let sol = solve (gen_sys g) in
  fun vn -> fst sol ("Gen",vn)

let check_gen : grammar -> bexpr =
 fun g -> VNSet.fold (fun vn c -> And(BVar (genv vn),c)) (nterminals g) (Bool true)

(** Null *)

let nullv : vn -> var  =
 fun vn -> ("Null",vn)

let rec null_prod :  v list -> bexpr =
 fun  l -> match l with
  | [] -> Bool true
  | Vt _ :: l -> Bool false
  | Vn v :: l -> And(BVar (nullv v), null_prod l)

let null_vn   : grammar -> vn -> bexpr =
 fun g vn -> let ls = (production g vn) in
    List.fold_left (fun acc vs -> Or(acc,null_prod vs)) (Bool false) ls

let null_sys : grammar -> equation list =
 fun g -> make_bool_system g "Null" null_vn

let null : grammar -> vn -> bool =
 fun g  ->
  let sol = solve (null_sys g) in
  fun vn -> fst sol  (nullv vn)

(** First *)
let firstv : vn -> var =
 fun vn -> ("First",vn)

let rec first_prod : v list -> sexpr =
 fun  l ->  match l with
  | [] -> Set VTSet.empty
  | Vt v :: l -> Set (VTSet.singleton v)
  | Vn v :: l -> Union (SVar (firstv v), Cond (BVar (nullv v), (first_prod l)))

let first_vn : grammar ->  vn -> sexpr =
 fun g vn -> List.fold_left
    (fun sexp vl -> Union ((first_prod vl), sexp))
    (Set VTSet.empty)
    (production g vn)

let first_sys : grammar -> equation list =
 fun g -> make_set_system g "First"  first_vn

let first : grammar ->  vn -> VTSet.t =
 fun g  ->
  let sol = solve ((null_sys g) @(first_sys g)) in
  fun vn -> snd sol  ("First",vn)

(** Follow *)
let followv : vn -> var =
 fun vn ->  ("Follow",vn)

let follow_vn : grammar ->  vn -> sexpr =
 fun g vn -> let ls = gather_sprod_using_vn g vn in
 List.fold_left 
    (fun acc vs -> Union(acc, Union ((first_prod vs.wordr), Cond((null_prod vs.wordr), SVar(followv vs.ntl)))))
    (Set VTSet.empty)
    ls

let follow_sys : grammar -> equation list =
 fun g -> make_set_system g "Follow" follow_vn

let follow : grammar -> vn -> VTSet.t =
 fun g ->
  let sol = solve ((null_sys g) @(first_sys g)@(follow_sys g)) in
  fun vn -> snd sol ("Follow",vn)

(** Checking LL1 *)

(*TODO*)
let ll1_vn : grammar -> vn -> bexpr =
    fun g vn ->
        let rec change_in_list v =
            match v with
            |a -> a :: []
            |Union(a,b) -> (change_in_list a) @ (change_in_list b) in
        let rec paire a l =
            match l with
            |[] -> []
            |h :: t -> Union(a, h) :: (paire a t) in 
        let rec make_paire l =
            match l with
            |[] -> []
            |h :: t -> (paire h t) @ (make_paire t) in 
        let rec inter_list l =
            match l with
            |[] -> Set VTSet.empty
            |h::[] -> h
            |h::t -> (Inter(h, inter_list t)) in
        let ls = first_vn g vn in
    IsEmpty (inter_list (make_paire (change_in_list ls)))

let is_ll1 : grammar  -> bexpr =
      fun g -> 
            let vns = nterminals  g in
              VNSet.fold (fun vn e -> And(ll1_vn g vn,e))  vns (Bool true)

(** Derivation *)

let is_first : solution -> vn -> VTSet.t =
 fun sol v -> snd sol (firstv v)

let is_null : solution -> vn -> bool =
 fun sol v -> fst sol (nullv v)

let is_follow : solution -> vn -> VTSet.t =
 fun sol v -> snd sol (followv v)

let deriv : grammar -> vn -> vt -> (v list) option =
  fun g ->
   let sol = solve ((null_sys g) @(first_sys g)@(follow_sys g)) in
   fun vn vt ->
   let prods = production g vn in
   (* TODO *) None
