type constr = string
type symbol = string
type tyexpr = string

type prec = 
	| PToken of constr
	| PTToken of tyexpr * constr
	| PStart of symbol
	| PType of tyexpr * symbol
	| PLeft of symbol list
	| PRight of symbol list
	| PNonassoc of symbol list

type code = string
type action = code * (int list)

type nonterminal = string
type rule = nonterminal * (symbol list * action) list

type header = string
type top = header * (prec list) * (rule list)

