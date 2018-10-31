open Env.Environment
exception InvalidExpression

type value =
  | Num of float
  | Bool of bool
  | List of value list
  
type expr =
  | ConstExp of float
  | DiffExp of expr * expr
  | IsZeroExp of expr
  | IfExp of expr * expr * expr
  | VarExp of string
  | LetExp of string * expr * expr
  | MinusExp of expr
  | EmptyListExp
  | ConsExp of expr * expr
  | CarExp of expr
  | CdrExp of expr
  | IsNullExp of expr

let rec string_of_value v = match v with
  | Num f -> string_of_float f
  | Bool b -> string_of_bool b
  | List xs -> "[" ^ (String.concat "," (List.map (fun x -> string_of_value x) xs)) ^ "]"  

module Value_env = Make_env(struct type t = value end)
open Value_env

let rec value_of e env =
  match e with
  | ConstExp v -> Num v
  | DiffExp (e1,e2) -> diff e1 e2 env
  | IsZeroExp e1 -> is_zero e1 env
  | IfExp (e1,e2,e3) -> conditional e1 e2 e3 env
  | VarExp v ->
      (match (apply_env env v) with
      | None -> raise InvalidExpression
      | Some v -> v)
  | LetExp (v,e1,e2) ->
      value_of e2 (extend_env v (value_of e1 env) env)
  | MinusExp e1 -> diff (ConstExp 0.) e1 env
  | EmptyListExp -> List []
  | ConsExp (e1,e2) -> eopl_list e1 e2 env
  | CarExp e1 -> car e1 env
  | CdrExp e1 -> cdr e1 env
  | IsNullExp e1 -> is_null e1 env

and diff e1 e2 env =
  let v1 = value_of e1 env in
  let v2 = value_of e2 env in
  match (v1,v2) with
  | (Num x, Num y) -> Num (x -. y)
  | _ -> raise InvalidExpression

and is_zero e env =
  let v = value_of e env in
  match v with
  | Num v -> Bool (v = 0.)
  | _ -> raise InvalidExpression

and conditional e1 e2 e3 env =
  let v1 = value_of e1 env in
  match v1 with
  | Bool b ->
    if b then value_of e2 env else value_of e3 env
  | _ -> raise InvalidExpression

and eopl_list e1 e2 env =
  let rest = value_of e2 env in
  match rest with
  | List ls -> List ((value_of e1 env) :: ls)
  | _ -> raise InvalidExpression

and car e1 env =
  let v = value_of e1 env in
  match v with
  | List (x::_) -> x
  | _ -> raise InvalidExpression

and cdr e1 env =
  let v = value_of e1 env in
  match v with
  | List (_::xs) -> List xs
  | _ -> raise InvalidExpression

and is_null e1 env =
  let v = value_of e1 env in
  match v with
  | List xs -> Bool (xs = [])
  | _ -> raise InvalidExpression


let run_program e =
  let env = empty_env () in
  value_of e env

