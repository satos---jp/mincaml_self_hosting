let rec f i = 
	if i > 126 then () 
	else (
		print_string ("x" ^ (Char.escaped (Char.chr i)) ^ "x");
		print_char 10
	)


;;

f 32

