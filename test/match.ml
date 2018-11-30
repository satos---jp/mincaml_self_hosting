type rule_state = 
	| A
	| B

let rec stats2str ss = 
	match ss with
	| A :: xs -> "Going" ^ (stats2str xs)
	| B :: xs -> "Gone" ^ (stats2str xs)
	| [] -> ""

let ss = [A;B;B]


let f a b = 
	match (a,b) with
	| x :: xs, y :: ys -> x + y

;;

print_string (stats2str ss);
print_char (f [3;4;5] [6;7;8]);
print_char 10

