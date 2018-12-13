(* Parser.specification_list Lexer.main (Lexing.from_channel ic) が動けばよい *)

(*
{ header }
let ident = regexp ...
rule entrypoint [arg_1... arg_n] =
  parse regexp { action }
      | ...
      | regexp { action }
and entrypoint [arg_1... arg_n] =
  parse ...
and ...
{ trailer }

こうらしいが、 header と trailer はいらないっぽいので無視します

あと、型推論もしくはimportの関係から とりあえず arg_1... argn はなしで...

んー、mliでexportする際に型推論しないといけなくなってきたぞ、
というか コンパイル時に推論される型から mli を変更する必要が出てきたぞ...?
まぁこの辺の話は一旦見なかったことにします。(コンパイル後に適宜手で型をつけることにする)
*)

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r'
let alpha = ['a'-'z' 'A'-'Z' '_' ] 
let smallalpha = ['a'-'z' '_' ] 
let bigalpha = ['A'-'Z'] 
let ident = smallalpha (alpha | digit)* 

(* スプチャってスプラチャージャーぽくないですか *)
let spcha = ['\'' '\\' '+' '*' '/' '-' '=' '.' '<' '>' '(' ')' '{' '}' ';' '^' ':' '\'' '|' '[' ']' '^']

let string_chars = (digit|alpha|' '|spcha)
let char_chars = (digit|alpha|' '|spcha|'"')

rule main = parse
| "\n" { Lexing.new_line lexbuf; main lexbuf }
| space+       { main lexbuf }
| "(*"         { comment lexbuf }
| '"' (string_chars* as str) '"' { Parser.STRING str }
| '\'' (('\\' char_chars | char_chars) as str) '\'' { Parser.CHAR str }
(* TODO(satos) ここ、あとで ('\\'? char_chars) に戻したい *)
(*
| '{' (string_chars* as str) '}' { Parser.CODE str }
*)
| '{'          { Parser.CODE(code lexbuf) }
| "let"        { Parser.LET }
| "="          { Parser.EQ }
| "rule"       { Parser.RULE }
| "parse"      { Parser.PARSE }
| "and"        { Parser.AND }
| "as"         { Parser.AS }
| "|"          { Parser.BAR }
| "("          { Parser.LPAR }
| ")"          { Parser.RPAR }
| "["          { Parser.LBRA }
| "]"          { Parser.RBRA }
| "*"          { Parser.STAR }
| "-"          { Parser.MINUS }
| "_"          { Parser.UNSCO }
| "+"          { Parser.PLUS }
| ident  as id { Parser.ID id }
| eof          { Parser.EOF }
| (_ as s)     { failwith ("Unknown Token: " ^ (Char.escaped s))}

and comment = parse
| "\n" { Lexing.new_line lexbuf; comment lexbuf }
| "*)"       { main lexbuf }
| _          { comment lexbuf }

and code = parse
| "\n" { Lexing.new_line lexbuf; "\n" ^ (code lexbuf) }
| "}"       { "" }
| _  as c   { (String.make 1 c) ^ (code lexbuf) }



