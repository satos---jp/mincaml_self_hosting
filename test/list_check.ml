let v = [1;2;3]
let w = List.map (fun x -> x + 3) v 

let rec print_list xs = 
	List.iter (fun x -> 
		print_int x;
		print_char 10
	) xs


;;

print_list w;
print_list (v @ w)

