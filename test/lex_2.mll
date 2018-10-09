(*
let digit = ['0'-'9']

let alpha = ['a'-'z' 'A'-'Z' '_' ] 
let ident = alpha (alpha | digit)*
*)

let digit = ['0'-'3']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'c'] 
let ident = alpha (alpha | digit)*

rule main = parse
| "END"        { "END" }
| space+       { main lexbuf }
| digit+ as n  { "digit with " ^ n ^ " END" }
| ident  as id { "ident with " ^ id ^ " END" }
(*
| _            { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}
*)

