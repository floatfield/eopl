let parse lexbuf =
  Parser.prog Lexer.read lexbuf

let filename = Sys.argv.(1)

exception InvalidExpression

let in_c = open_in filename
let expr = match (parse (Lexing.from_channel in_c)) with
  | None -> raise InvalidExpression
  | Some e -> e

let res = Ast.run_program expr

let () =
  Printf.printf "%s\n" (Ast.string_of_value res)