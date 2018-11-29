let f x y = (fun z w -> x + y + z + w)
let g gf y = ((gf 4) 5) + y

let p1 a b = 
	print_string a;
	print_int b

let p2 a = fun b -> 
	print_string a;
	print_int b


;;
print_int ((f 2 3) 4 5);
print_int (((f 2) 3) 4 5);
print_int ((((f 2) 3) 4) 5);
print_int (g (fun p q -> p + q) 2);

p1 "hoge " 3;
(p1 "hoge ") 3;
p2 "hoge " 3;
(p2 "hoge ") 3;

print_int (f 2 3 4 5);
()


