let rec print_list w = 
	let rec f v = 
		match v with
		| x :: xs -> (
				print_char (x + 48);
				print_char 32;
				f xs
			)
		| [] -> ()
	in
		print_char 91;
		f w;
		print_char 93

let _ = (
	let v = 4 :: 3 :: 5 :: [] in
	print_list v;
	print_char 10
)

let rec append xs ys =
	match xs with
	| x :: xs -> x :: (append xs ys)
	| [] -> ys

let rec map f w = 
	match w with
	| x :: xs -> (f x) :: (map f xs)
	| [] -> []

let rec flatten v = 
	match v with 
	| x :: xs -> append x (flatten xs)
	| [] -> []

let rec ccat a v = 
	match v with 
	| x :: xs -> (a :: v) :: (map (fun p -> x :: p) (ccat a xs))
	| [] -> (a :: []) :: []

let rec perm v = 
	match v with
	|	x :: xs -> flatten (map (fun t -> ccat x t) (perm xs))
	| [] -> [] :: []


let rec print_list_list v = 
	match v with
	| x :: xs -> (
		  print_list x;
		  print_char 10;
			print_list_list xs
		)
	| [] -> 3

let _ = (
	let v = 1 :: 2 :: 3 :: [] in
	print_list_list (perm v)
)


