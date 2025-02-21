
let ws = [' ' '\t' '\r' '\n']*

rule token = parse
| ws     { Lexing.lexeme lexbuf; () }
