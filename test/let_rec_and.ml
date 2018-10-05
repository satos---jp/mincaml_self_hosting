let rec f x = 
	if x = 0 then true else (g (x-1))
and g x = 
	if x = 0 then false else (f (x-1))


;;
	print_int (if f 3 then 1 else 0);
	print_int (if f 8 then 1 else 0);
	print_int (if g 3 then 1 else 0);
	print_int (if g 8 then 1 else 0);
	print_char 10

