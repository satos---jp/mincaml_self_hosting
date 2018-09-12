let a = read_int () in
let b = read_int () in
print_int (a+b);
print_char 10;
let p = read_int () in
let q = read_int () in
print_int (p+q);
print_char 10;
let c = read_float () in
let d = read_float () in
print_int (int_of_float (c *. d));
print_char 10;
let r = read_float () in
let s = read_float () in
print_int (int_of_float (r +. s));
print_char 10;
print_int (int_of_float ((float_of_int p) +. r));
print_char 10

