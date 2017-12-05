let rec f x = 
	if x > 10.0 then () else (
	print_int (int_of_float (floor x));
	f (x +. 0.1))
in
f (-10.0)

