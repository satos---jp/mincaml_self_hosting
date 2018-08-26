type a = Hoge | Huga

let rec f x = 
	match x with
	| Hoge -> 65
	| Huga -> 48

type b = 
	| Piyo of int
	| Nyan of int * int

let rec g x = 
	match x with
	| Piyo t -> t + 2
	| Nyan(t,u) -> t - u + 6


let _ = (
	print_char (f Hoge); print_char 10;
	print_char (f Huga); print_char 10;
	print_char (g (Piyo 48)); print_char 10;
	print_char (g (Nyan(100,7))); print_char 10;
	print_char 10
)


type c = H
type d = Y of int * c

;;
Y(3,H)

