
let ws = [' ' '\t' '\r' '\n']*

rule token = parse
| ws     { () }
