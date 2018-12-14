
let rec concat s vs = 
	match vs with
	| [] -> ""
	| [x] -> x
	| x :: xs -> x ^ s ^ (concat s xs)

let make n c =
	let cs = Char.escaped c in
	let rec f x = 
		if x <= 0 then "" else cs ^ (f (x-1))
	in
		f n

let rec sub s a b = 
	if b <= 0 then "" else
	(make 1 (String.get s a)) ^ (sub s (a+1) (b-1))
