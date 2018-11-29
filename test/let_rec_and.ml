let rec f x = 
	print_string "f ";
	print_char (x+48);
	print_char 10;
	if x = 0 then () else (g (x-1))
and g x = 
	print_string "g ";
	print_char (x+48);
	print_char 10;
	if x = 0 then () else (h (x-1))
and h x = 
	print_string "h ";
	print_char (x+48);
	print_char 10;
	if x = 0 then () else (f (x-1))

;;
f 10;
g 20;
h 30

