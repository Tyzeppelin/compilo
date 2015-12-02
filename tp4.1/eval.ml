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
    | Val_int(a), Val_int(b) ->begin
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

let rec eval env = function
  | Ml_int n -> Val_int n
  | Ml_bool b -> Val_bool b
  | Ml_nil -> Val_nil
  | Ml_cons(e1, e2) -> Val_cons (eval env e1, eval env e2)
  | Ml_pair(p1, p2) -> Val_pair (eval env p1, eval env p2)
  | Ml_unop(op, exp)-> (eval_unop op (eval env exp))
  | Ml_binop(op, exp1, exp2) -> (eval_binop op (eval env exp1) (eval env exp2))
  | Ml_var v -> begin
     try
        List.assoc v env
    with Not_found -> raise (Error ("unbound variable " ^ v))
  end
  | Ml_if(cond, th, el) -> begin
    match (eval env cond) with
    | Val_bool true -> eval env th
    | Val_bool false -> eval env el
    | _ -> raise (Error "wrong type, bool expected (if)")
  end
  | Ml_fun pattern_list -> Val_closure(env, pattern_list)
  | Ml_app(e1, e2) -> begin
    let val_arg = eval env e2 in
      match eval env e1 with
        | Val_closure(env_fun, pattern_list) -> begin
            try
                let (pattern_env, e) = tryfind (fun (pattern, e) -> (pattern_matching pattern val_arg, e)) pattern_list
                    in eval (pattern_env @ env_fun) e
            with Not_found -> raise (Error "can't match any pattern (application)")
        end
        | _ -> raise (Error "type error, expected closure (aplication)")
  end
  | Ml_let (id, exp, exp_in) -> let v = (eval env exp) in (eval ((id, v)::env) exp_in)
  | Ml_letrec (f, exp, exp_in) -> begin
      match exp with
      | Ml_fun pattern_list -> let rec env2 = (f, Val_closure(env2, pattern_list)) :: env
            in (eval env2  exp_in)
      | _ -> raise (Error "bad recursion : illegal declaration !")
    end
  | _ -> raise (Error "not implemented yet!")

