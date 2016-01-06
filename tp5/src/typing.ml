open Print_ast
open Ast

let is_list_typ = function
 | TList _ -> true
 | _       -> false


let rec typ_of_pattern : ml_pattern -> TypEnv.t * Ast.typ =
 function
 | Ml_pattern_var(s,typ) -> TypEnv.singleton s typ, typ
 | Ml_pattern_bool b -> TypEnv.empty, Tbool
 | Ml_pattern_int i  -> TypEnv.empty, Tint
 | Ml_pattern_pair(p1,p2) ->
    let e1,t1 = typ_of_pattern p1 in
    let e2,t2 = typ_of_pattern p2 in
    TypEnv.add_all e1 e2 , TPair(t1,t2)
 | Ml_pattern_nil ty -> if is_list_typ ty then TypEnv.empty , ty else failwith "Type error"
 | Ml_pattern_cons(x,l) ->
    let e1,t1 = typ_of_pattern x in
    let e2,t2 = typ_of_pattern l in
        if t2 = TList t1
        then TypEnv.add_all e1 e2, t2
        else failwith (Printf.sprintf "Pattern type mismatch: %s expected but %s found." (string_of_typ t2) (string_of_typ (TList t1)))

let rec same_types l =
    match l with
    | [] -> failwith "Function error: Should have at least one expression"
    | e::le ->
        if List.for_all (fun ty -> ty = e) l
        then e
        else
            let m = List.find(fun ty -> ty <> e) l in
            failwith (Printf.sprintf "Function expression error: an expression was expected of type %s but %s found" (string_of_typ e) (string_of_typ m))

let rec wt_expr (env:TypEnv.t) = function
 | Ml_int i -> Tint
 | Ml_bool b -> Tbool
 | Ml_nil ty ->
    if is_list_typ ty
    then ty
    else failwith (Printf.sprintf "Type error: %s should be an 'a list'" (string_of_typ ty))
 | Ml_pair(e1,e2) ->
    let t1 = wt_expr env e1 in
    let t2 = wt_expr env e2 in
    TPair(t1,t2)
 | Ml_cons(e1,le1) ->
    let t1 = wt_expr env e1 in
    let t2 = wt_expr env le1 in
        if t2 = TList t1
        then t2
        else failwith (Printf.sprintf "Type mismatch : %s expected but %s found." (string_of_typ t1) (string_of_typ t2))
 | Ml_unop(op,e) ->
    let t = wt_expr env e in
    begin
     match op,t with
     |Ml_fst, TPair(x,y) -> x
     |Ml_snd, TPair(x,y) -> y
     | _ -> failwith (Printf.sprintf "Unop error: Op %s does not expect type %s." (string_of_unop op) (string_of_typ t))
 end
 | Ml_binop(op,e1,e2) -> begin
     let t1 = wt_expr env e1 in
     let t2 = wt_expr env e2 in
     match op,t1,t2 with
     | (Ml_add | Ml_sub | Ml_mult)  , Tint , Tint ->  Tint
     | Ml_less , Tint , Tint ->  Tbool
     | Ml_eq , x, y ->
        if x = y
        then Tbool
        else failwith (Printf.sprintf "Equal error: right side was expected to be %s but %s found." (string_of_typ x) (string_of_typ y))
     | _ -> failwith (Printf.sprintf "Binop error: Op %s waiting members to be \"int\" but found %s %s" (string_of_binop op) (string_of_typ t1) (string_of_typ t2))
 end
 | Ml_var x -> TypEnv.find x env
 | Ml_if(e1,e2,e3) ->
     let t1 = wt_expr env e1 in
     let t2 = wt_expr env e2 in
     let t3 = wt_expr env e3 in
     if t1 = Tbool
     then if t2 = t3
        then t2
        else failwith (Printf.sprintf "Type Mismatch: Else branch should be %s type but %s found." (string_of_typ t2) (string_of_typ t3))
     else failwith (Printf.sprintf "Boolean error: If condition type must be a boolean, but %s found" (string_of_typ t1))
 | Ml_fun l ->
     let ltyp = List.map (fun (p,e) ->
        let (envp,tp) = typ_of_pattern p in
     TFun(tp, wt_expr (TypEnv.update_all envp env) e)) l in
     same_types ltyp
 | Ml_app(e1,e2) ->
    let t1 = wt_expr env e1 in
    let t2 = wt_expr env e2 in
    begin
        match t1 with
        | TFun(tp,te) -> if t2 = te then te else failwith (Printf.sprintf "Type error: cannot apply function of type %s with argument of type %s" (string_of_typ t1) (string_of_typ t2))
        | _ -> failwith (Printf.sprintf "Type error: term has type %s and cannot be applied with argument of type %s" (string_of_typ t1) (string_of_typ t2))
    end
 | Ml_let (x,e1,e2) -> failwith "TODO"
 | Ml_letrec(x,typ,e1,e2) -> failwith "TODO"

let wt_ast tenv ast =
  match ast with
  | Ml_expr e -> wt_expr (!tenv) e
  | Ml_definition(s,e) ->
   let ty' = wt_expr !tenv e in
   tenv := TypEnv.update s ty'  !tenv ;
   ty'
  | Ml_definitionrec (s,ty',e) ->
   let ty = wt_expr (TypEnv.update s ty' !tenv) e in
   if ty = ty'
   then
    begin
     tenv := TypEnv.update s ty !tenv ;
     ty'
    end
   else failwith (Printf.sprintf "Type error: let rec with incompatible types %s and %s" (string_of_typ ty) (string_of_typ ty'))




