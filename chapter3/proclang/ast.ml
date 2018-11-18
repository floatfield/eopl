exception InvalidExpression

type expr =
  | ConstExp of float
  | DiffExp of expr * expr
  | IsZeroExp of expr
  | IfExp of expr * expr * expr
  | VarExp of string
  | LetExp of string * expr * expr
  | ProcExp of string * expr
  | CallExp of expr * expr
  
type value =
  | Num of float
  | Bool of bool
  | Proc of string * expr * environment

and environment = (string * value) list


let empty_env: unit -> environment = fun () -> []
let extend_env key value env=
  (key, value) :: env
let rec apply_env env key =
  match env with
  | [] -> None
  | (k,v) :: env1 ->
    if k = key then
      Some v
    else
      apply_env env1 key


let string_of_value v = match v with
  | Num f -> string_of_float f
  | Bool b -> string_of_bool b
  | Proc _ -> "proc goes here"

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
  | ProcExp (v, e1) -> Proc (v,e1,env)
  | CallExp (v, e1) -> call_expr v e1 env

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

and call_expr rator rand env =
  let proc = value_of rator env in
  let arg = value_of rand env in
  match proc with
  | Proc (var,body,env1) -> value_of body (extend_env var arg env1)
  | Num _ | Bool _ -> raise InvalidExpression


let run_program e =
  let env = empty_env () in
  value_of e env
