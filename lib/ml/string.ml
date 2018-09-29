
let rec concat s vs = 
	match vs with
	| [] -> ""
	| x :: xs -> x ^ s ^ (concat s xs)

