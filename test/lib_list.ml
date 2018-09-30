let rec read_list _ = 
	let c = read_int () in
	if c < 0 then []
	else c :: (read_list ())


let rec print_list v = 
	match v with
	| [] -> print_char 10
	| x :: xs -> (
			print_int x;
			print_char 32;
			print_list xs
		)

let v = read_list ()
let a = read_int ()
let b = read_int ()
;;

print_int (if List.mem a v then 1 else 0);
print_int (if List.mem b v then 1 else 0);
print_char 10

