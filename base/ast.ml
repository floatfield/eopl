open Env.Environment
exception InvalidExpression

type value =
  | Num of float
  | Bool of bool
  
type expr =
  | ConstExp of float
  | DiffExp of expr * expr
  | IsZeroExp of expr
  | IfExp of expr * expr * expr
  | VarExp of string
  | LetExp of string * expr * expr
    
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


let run_program e =
  let env = empty_env () in
  value_of e env

