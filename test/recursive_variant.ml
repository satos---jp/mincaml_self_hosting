type nat = Z | S of nat

let rec add m n =
	match m with 
	| Z -> n 
	| S x -> S (add x n)

let rec sub m n = 
	match n with
	| Z -> m
	| S x -> 
		match m with
		| Z -> Z
		| S y -> sub y x

let rec mul m n =
	match m with 
	| Z -> Z
	| S x -> add n (mul x n) 

let rec pow m n =
	match m with 
	| Z -> S Z
	| S x -> mul n (pow x n) 

let rec n2i n =
	match n with
	| Z -> 0
	| S x -> (n2i x) + 1

let rec i2n x =
	if x = 0 then Z else S (i2n (x-1))

let _ = (
	print_char (48 + n2i (S (S (S Z))));
	print_char (48 + n2i (i2n 5));
	print_char (48 + n2i (add (i2n 5) (i2n 3)));
	print_char (48 + n2i (sub (i2n 5) (i2n 3)));
	print_char (n2i (pow (i2n 3) (i2n 5)));
	print_char 10
)



type tree = 
	| Leaf
	| Node of int * tree * tree

let rec pre_order t p = 
	match t with
	| Node (a,b,c) -> a + (pre_order b (p+1)) +  (pre_order c (p+2)) 
	| Leaf -> p

let _ = (
	print_char ((pre_order (Node(3,Leaf,Leaf)) 1) + 65);
	print_char ((pre_order (Node(2,Node(3,Leaf,Leaf),Leaf)) 1) + 65);
	print_char 10
)


