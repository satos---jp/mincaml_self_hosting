
(* Ocamlのlexerのhogeな仕様のせいで、pos_lnum と pos_bol は自分で更新しないといけない!!  *)


(* 参考 http://mamewo.ddo.jp/ml.html の lexing_module の項 *)



let digit = ['0'-'9']
let space = ' ' | '\t' | '\r'
let alpha = ['a'-'z' 'A'-'Z' '_' ] 
let smallalpha = ['a'-'z' '_' ] 
let bigalpha = ['A'-'Z'] 
let ident = smallalpha (alpha | digit)* 
let variant_ident = bigalpha (alpha | digit)* 

let string_chars = (digit|alpha|' ')

rule main = parse
| "\n" { Lexing.new_line lexbuf; main lexbuf }
| space+       { main lexbuf }
| "(*"         { comment lexbuf }
| '"' (string_chars* as str) '"' { Parser.STRING str }
| "let"        { Parser.LET }
| "rec"        { Parser.REC }
| "in"         { Parser.IN }
| "if"         { Parser.IF }
| "then"       { Parser.THEN }
| "else"       { Parser.ELSE }
(*
min-rt の先頭に、 
let true = 1 とかがあるのでエラーになる。
-> Boolは消しました。
| "true"       { Parser.BOOL (true) }
| "false"      { Parser.BOOL (false) }
*) 
| "create_array" { Parser.ALLCREATE } 
| "("          { Parser.LPAR }
| ")"          { Parser.RPAR }
| "fun"        { Parser.FUN }
| "not"        { Parser.NOT }
| "<-"         { Parser.LARROW }
| "->"         { Parser.RARROW }
| "."          { Parser.DOT  }
| ","          { Parser.COMMA  }
| ";;"         { Parser.SEMISEMI }
| ";"          { Parser.SEMI }
| "+."          { Parser.FPLUS }
| "-."          { Parser.FMINUS }
| "*."          { Parser.FTIMES }
| "/."          { Parser.FDIV }
| "+"          { Parser.PLUS }
| "-"          { Parser.MINUS }
| "*"          { Parser.TIMES }
| "/"          { Parser.DIV }
| "="          { Parser.EQ }
| "<>"         { Parser.NEQ }
| "<="         { Parser.LEQ }
| "<"          { Parser.LT }
| ">="         { Parser.GEQ }
| ">"          { Parser.GT }
| "|"          { Parser.BAR }
| "type"       { Parser.TYPE }
| "match"      { Parser.MATCH }
| "with"       { Parser.WITH }
| "of"         { Parser.OF }
| "::"         { Parser.CONS }
| "val"        { Parser.VAL }
| ":"          { Parser.COLON }
| "open"       { Parser.OPEN }
| "'"          { Parser.QUOTE }
| "["          { Parser.LBRACKET }
| "]"          { Parser.RBRACKET }
| "@"          { Parser.ATMARK }
| "^"          { Parser.CARET }
| digit+ "." digit*(['E''e']['+''-']?digit+)? as f { Parser.FLOAT(float_of_string f) }
| digit+ as n  { Parser.INT (int_of_string n) }
| ident  as id { Parser.ID id }
| variant_ident as id { Parser.VARIANT_ID id }
| eof          { Parser.EOF }
| _            { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}

and comment = parse
| "\n" { Lexing.new_line lexbuf; comment lexbuf }
| "*)"       { main lexbuf }
| _          { comment lexbuf }


