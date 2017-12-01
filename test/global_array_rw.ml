let gv = create_array 2 6 in
let rec get p = gv.(p)
in 
let rec set p q = gv.(p) <- q 
in 
print_int (get 1);
set 1 7;
print_int (get 1);
print_int (get 0);
print_char 10

