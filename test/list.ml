let rec read_list _ = 
	let c = read_char () in
	if c = 10 then []
	else c :: (read_list ())


let rec print_list v = 
	match v with
	| [] -> print_char 10
	| x :: xs -> (
			print_char x;
			print_char 32;
			print_list xs
		)
	
