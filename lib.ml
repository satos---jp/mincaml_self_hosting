let rec mod_ a b = a-(a/b)*b
let rec print_int_base x = 
	if x < 10 then (if 0 < x then print_char (x+48) else ()) else
		(print_int_base (x/10);
		print_char ((mod_ x 10)+48))

let rec print_int x = 
	if x = 0 then print_char 48
	else (if x < 0 then print_char 45; print_int_base (0-x) else print_int_base x)


