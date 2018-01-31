let rec maina _ = 
	let rec f t = 
		let (x,y) = t in x - y
	in
		let p = (30,6) in
		print_int (f p);
		print_char 10
in 
let rec mainb _ = 
	let rec f a b = 
		(a,b)
	in
		let (q,r) = f 30 6 in
		print_int q;
		print_char 10
in 
	maina ();
	mainb ()

