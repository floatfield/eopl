{
    open Parser
    exception SyntaxError of string
}
let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let frac = '.' digit*
let float = digit* frac?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let iden = alpha (alpha|digit)*

rule read =
    parse
    | white { read lexbuf }
    | newline { read lexbuf }
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | "zero?" { ZERO }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "let" { LET }
    | "in" { IN }
    | "minus" { MINUS }
    | "emptylist" { EMPTYLIST }
    | "cons" { CONS }
    | "car" { CAR }
    | "cdr" { CDR }
    | "null?" { ISNULL }
    | iden { ID (Lexing.lexeme lexbuf) }
    | '-' { DASH }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '=' { EQ }
    | ',' { COMMA }
    | eof { EOF }
    | _ { raise (SyntaxError "Something went wrong :(") }