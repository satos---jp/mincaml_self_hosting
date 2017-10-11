let rec f t = 
	let (x,y) = t in x + y
in
	let p = (2,3) in
	print_int (f p);
	print_char 10

