module type Value = sig
  type t
end

module type Env = sig
  type t
  type v

  val empty_env: unit -> t
  val extend_env: string -> v -> t -> t
  val apply_env: t -> string -> v option
end

module Make_env(Val: Value):
  Env with type v := Val.t =
struct
  type t = (string * Val.t) list

  let empty_env: unit -> t = fun () -> []

  let extend_env key value env: t =
    (key, value) :: env
  
  let rec apply_env env key =
    match env with
      | [] -> None
      | (k,v) :: env1 ->
        if k = key then
          Some v
        else
          apply_env env1 key
end