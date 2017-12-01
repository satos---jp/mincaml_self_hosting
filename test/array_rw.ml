let n = read_int () in
let m = read_int () in
let x = create_array n m in
let p = read_int () in
let rec f t = 
	if t >= 0 then 
		x.(t) <- (t + p - 2);
		f (t-1)
	else ()
in
f (n-3);
let rec g t = 
	if t >= 0 then
		print_int x.(t);
		g (t-1)
	else ()
in
g (n-1)

