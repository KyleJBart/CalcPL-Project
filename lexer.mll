{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+ '.' digit+ (* NEW *)
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "<=" { LEQ }
  | ">=" { GEQ } (* NEW *)
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { MINUS } (* NEW *)
  | "/" { DIV } (* NEW *)
  | "+." { FPLUS } (* NEW *)
  | "*." { FTIMES } (* NEW *)
  | "-." { FMINUS } (* NEW *)
  | "/." { FDIV } (* NEW *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | ":" {COLON}
  | "int" {INT_TYPE}
  | "bool" {BOOL_TYPE}
  | "float" {FLOAT_TYPE} (* NEW *)
  | id { ID (Lexing.lexeme lexbuf) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) } (* NEW - before int *)
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }