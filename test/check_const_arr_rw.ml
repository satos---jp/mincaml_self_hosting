let rec main _ = 
	let v = create_array 10 0 in
	let rec f x = 
		v.(1) <- x;
		v.(1)
	in
	print_int (f 6);
	print_char 10
in 
	main ()

