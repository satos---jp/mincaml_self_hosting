let v = 1 :: 2 :: 3 :: []
let w = List.map (fun x -> x + 3) v 

;;

List.iter (fun x -> 
	print_int x;
	print_char 10
) w

