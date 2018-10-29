let parse str =
  let lexbuf = Lexing.from_string str in
  Parser.prog Lexer.read lexbuf

let text = "let x = 3 in -(x,4)"

exception InvalidExpression

let expr = match (parse text) with
  | None -> raise InvalidExpression
  | Some e -> e

let res = match (Ast.run_program expr) with
| Ast.Num x -> string_of_float x
| Ast.Bool b -> string_of_bool b

let () =
  Printf.printf "Hello, world!\n%s\n" res