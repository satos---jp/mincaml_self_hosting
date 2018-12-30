
let digit = ['0'-'9']

rule main = parse
| "+"  { Yacc_1.ADD }
| "-"  { Yacc_1.SUB }
| "*"  { Yacc_1.MUL }
| "/"  { Yacc_1.DIV }
| "\n" { Yacc_1.NL }
| "("  { Yacc_1.LPAR }
| ")"  { Yacc_1.RPAR }
| digit+ as n  { Yacc_1.INT (int_of_string n) }

