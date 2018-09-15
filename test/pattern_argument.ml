let f1 ((a,b),c) = a
let f2 ((a,b),c) = b
let f3 ((a,b),c) = c
let x = ((3,5),7)

;;
print_int (f1 x);
print_int (f2 x);
print_int (f3 x);
print_char 10

