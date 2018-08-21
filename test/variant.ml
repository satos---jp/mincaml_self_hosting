type a = Hoge | Huga

let f x = 
	match x with
	| Hoge -> 6
	| Huga -> 9

type b = 
	| Piyo of int
	| Nyan of int * int

let g x = 
	match x with
	| Piyo t -> t + 2
	| Nyan(t,u) -> t + u + 6

let _ = (
	print_int (f Hoge); print_char 10;
	print_int (f Huga); print_char 10;
	print_int (g (Piyo 2)); print_char 10;
	print_int (g (Nyan(5,7))); print_char 10;
	print_char 10
)


