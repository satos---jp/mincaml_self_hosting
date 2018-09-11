let rec map f v = 
	match v with
	| x :: xs -> (f x) :: (map f xs)
	| [] -> []

let rec iter f v =
	match v with
	| x :: xs -> (
			f x;
			iter f xs
		)
	| [] -> ()

let rec append v w = 
	match v with
	| x :: xs -> x :: (append xs w)
	| [] -> w

