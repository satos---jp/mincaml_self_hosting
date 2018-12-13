
let rec concat s vs = 
	match vs with
	| [] -> ""
	| x :: xs -> x ^ s ^ (concat s xs)

let make n c =
	let cs = Char.escaped c in
	let rec f x = 
		if x <= 0 then "" else cs ^ (f (x-1))
	in
		f n
