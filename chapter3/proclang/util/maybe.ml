let map f m = match m with
| Some x -> Some (f x)
| None -> None

let apply mf m = match (mf, m) with
| (Some f, Some x) -> Some (f x)
| _ -> None

let defaultWith x m = match m with
| Some y -> y
| None -> x

let from x = Some x

let (<$>) f m = map f m
let (<*>) mf m = apply mf m