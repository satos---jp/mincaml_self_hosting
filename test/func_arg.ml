let rec g y x = y - x in
let rec f x y = 
	g y x 
in
	print_int (f 10 65);
	print_char 10

