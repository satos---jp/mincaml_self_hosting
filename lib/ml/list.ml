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

let rec concat vs = 
	match vs with
	| x :: xs -> append x (concat xs)
	| [] -> []

let rec mem a v = 
	match v with
	| x :: xs -> if x = a then true else mem a xs
	| [] -> false


let rec fold_left f r v = 
	match v with
	| [] -> r
	| x :: xs -> f (fold_left f r xs) x

let rec mem_assoc x v = 
	match v with
	| [] -> false
	| (t,_) :: xs -> if t = x then true else mem_assoc x xs

let rec mapi f v = 
	let rec mapi_base i w = 
		match w with
		| [] -> []
		| x :: xs -> (f i x) :: mapi_base (i+1) xs
	in
		mapi_base 0 v

let rec nth v i = 
	match v with
	| [] -> (
			raise_match_failure "invalid List.nth length"
		)
	| x :: xs -> if i = 0 then x else nth xs (i-1) 

let rec length v = 
	match v with
	| [] -> 0
	| x :: xs -> (length xs) + 1
