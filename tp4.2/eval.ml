open Ast
open Value

exception Error of string

let eval_unop op arg = match arg with
    | Val_pair(v1, v2) -> begin
        match op with
        | Ml_fst -> v1
        | Ml_snd -> v2
    end
    |_ -> raise (Error "Bad Unop!")

let rec eval_eq arg1 arg2 =
    match arg1, arg2 with
    | Val_int a, Val_int b -> a = b
    | Val_bool a, Val_bool b -> a = b
    | Val_nil, Val_nil -> true
    | Val_nil, Val_cons _ -> false
    | Val_cons _, Val_nil -> false
    | Val_pair(a, b), Val_pair(c, d) -> (eval_eq a c) && (eval_eq b d)
    | Val_cons(a, b), Val_cons(c, d) -> (eval_eq a c) && (eval_eq b d)
    | _ -> raise (Error "wrong type (equal) !")

let eval_binop op arg1 arg2 = match arg1, arg2 with
    | Val_int(a), Val_int(b) -> begin
        match op with
        | Ml_add -> Val_int(a+b)
        | Ml_sub -> Val_int(a-b)
        | Ml_mul -> Val_int(a*b)
        | Ml_eq  -> Val_bool(a = b)
        | Ml_inf -> Val_bool(a < b)
        end
    |_ ->
        match op with
            | Ml_eq -> Val_bool(eval_eq arg1 arg2)
            | _ -> raise (Error "wrong type (Binop) !")

let rec pattern_matching pattern value = match pattern, value with
    | Ml_pattern_bool b, Val_bool a -> if b = a then [] else raise (Error "pattern matching fail (bool)")
    | Ml_pattern_int n, Val_int a -> if n = a then [] else raise (Error "pattern matching failure (int)")
    | Ml_pattern_nil, val_nil -> []
    | Ml_pattern_var id, v -> [(id, v)]
    | Ml_pattern_cons (c1, c2), Val_cons (d1, d2) -> (pattern_matching c1 d1) @ (pattern_matching c2 d2)
    | Ml_pattern_pair (p1, p2), Val_pair (q1, q2) -> (pattern_matching p1 q1) @ (pattern_matching p2 q2)
    | _ -> raise (Error "Can't match both types (pattern_matching) !")

let rec tryfind f = function
  | [] -> raise Not_found
  | hd :: tl -> try f hd with _ -> tryfind f tl

let rec eval_expr env = function
  | Ml_int n -> Val_int n
  | Ml_bool b -> Val_bool b
  | Ml_nil -> Val_nil
  | Ml_cons(e1, e2) -> begin
      match e2 with
        | Ml_cons(_,_) | Ml_nil -> Val_cons (eval_expr env e1, eval_expr env e2)
        | _ -> raise(Error "Bad list construction. Should've end with [] !")
  end
  | Ml_pair(p1, p2) -> Val_pair (eval_expr env p1, eval_expr env p2)
  | Ml_unop(op, exp)-> (eval_unop op (eval_expr env exp))
  | Ml_binop(op, exp1, exp2) -> (eval_binop op (eval_expr env exp1) (eval_expr env exp2))
  | Ml_var v -> begin
     try
        List.assoc v env
    with Not_found -> raise (Error ("unbound variable " ^ v))
  end
  | Ml_if(cond, th, el) -> begin
    match (eval_expr env cond) with
    | Val_bool true -> eval_expr env th
    | Val_bool false -> eval_expr env el
    | _ -> raise (Error "wrong type, bool expected (if)")
  end
  | Ml_fun pattern_list -> Val_closure(env, pattern_list)
  | Ml_app(e1, e2) -> begin
    let val_arg = eval_expr env e2 in
      match (eval_expr env e1) with
        | Val_closure(env_fun, pattern_list) -> begin
            try
                let (pattern_env, e) = tryfind (fun (pattern, e) -> (pattern_matching pattern val_arg, e)) pattern_list
                    in eval_expr (pattern_env @ env_fun) e
            with Not_found -> raise (Error "can't match any pattern (application)")
        end
        | _ -> raise (Error "type error, expected closure (aplication)")
  end
  | Ml_let (id, exp, exp_in) -> let v = (eval_expr env exp) in (eval_expr ((id, v)::env) exp_in)
  | Ml_letrec (f, exp, exp_in) -> begin
      match exp with
      | Ml_fun pattern_list -> let rec env2 = (f, Val_closure(env2, pattern_list)) :: env in
            (eval_expr env2  exp_in)
      | _ -> raise (Error "bad recursion : illegal declaration !")
  end
  (*|_ -> raise (Error "not implemented yet!")*)


let eval_def env = function
    | Ml_def_let(id, expr) -> let val_expr = (eval_expr !env expr) in
        env := (id, val_expr) :: !env; Val_nil
    | Ml_def_rec(id, expr) -> begin
        match expr with
            | Ml_fun pat -> let rec env2 = (id, Val_closure(env2, pat)) :: !env in
                env := env2 ;Val_nil
            | _ -> raise (Error "bad recursion : illegal definition !")
        end
    (*| _ -> raise (Error "not implemented yet !")*)

let eval env = function
    | Ml_expr expr -> eval_expr !env expr
    | Ml_def def -> eval_def env def
