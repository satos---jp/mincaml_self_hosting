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

