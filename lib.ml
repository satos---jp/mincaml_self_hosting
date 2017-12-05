let false = 0
let rec mod_ a b = a-(a/b)*b
let rec print_int_base x = 
	if x < 10 then (if 0 < x then print_char (x+48) else ()) else
		(print_int_base (x/10);
		print_char ((mod_ x 10)+48))

let rec print_int x = 
	if x = 0 then print_char 48
	else (if x < 0 then print_char 45; print_int_base (0-x) else print_int_base x)


(*


let rec read_int x = 
	let rec f acc = 
		let n = read_char () in
			if (n-47)*(58-n)>0 then f (acc*10+(n-48)) else acc
	in
		let c = read_char () in
			if c = 45 then -(f 0) else (
				if (c-47)*(58-c)>0 then f (c-48) else read_int x)

let rec read_float x = 
	let rec g b acc = 
		let n = read_char () in
			if (n-47)*(58-n)>0 then g (b *. 0.1) (acc+.b*.(float_of_int (n-48))) else acc
	in
	let rec f acc = 
		let n = read_char () in
			if (n-47)*(58-n)>0 then f (acc*.10.0+.(float_of_int (n-48))) else (
				if n = 46 then g 0.1 acc else acc)
	in
		let c = read_char () in
			if c = 45 then (0.0 -.(f 0.0)) else (
				if (c-47)*(58-c)>0 then f (float_of_int (c-48)) else read_float x)
*)


let rec print_hex_err_chr c = 
	if c < 10 then print_char_err (c+48) else print_char_err (c+87)

let rec print_hex_err_base x = 
	if x < 16 then (if 0 < x then print_hex_err_chr x else ()) else
		print_hex_err_base (x/16);
		print_hex_err_chr (mod_ x 16)

let rec print_hex_err x = 
	if x = 0 then print_char_err 48
	else (if x < 0 then print_char_err 45; print_hex_err_base (0-x) else print_hex_err_base x)



