let rec print_fi x = 
	print_int (int_of_float (x *. 1000000.0))
in 
let rec f x = 
	if fless 100.0 x then () else
	(print_fi x;
	print_char 32;
	print_fi (sin x);
	print_char 32;
	print_fi (cos x);
	print_char 32;
	print_fi (atan x);
	print_char 10;
	f (x +. 0.1))
in
	f (read_float ())

