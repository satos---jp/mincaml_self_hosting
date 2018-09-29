type rule_state = 
	| A
	| B

let rec stats2str ss = 
	match ss with
	| A :: xs -> "Going" ^ (stats2str xs)
	| B :: xs -> "Gone" ^ (stats2str xs)
	| [] -> ""

let ss = [A;B;B]

;;
print_string (stats2str ss);
print_char 10

