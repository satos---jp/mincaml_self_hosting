let rec mod_ a b = a-(a/b)*b in
let rec print_int_base x = 
	if x < 10 then (if 0 < x then print_char (x+48) else ()) else
		(print_int_base (x/10);
		print_char ((mod_ x 10)+48))
in
let rec print_int_ x = 
	if x = 0 then print_char 48
	else (if x < 0 then print_char 45; print_int_base (0-x) else print_int_base x)



in
let rec fib n =
  if n <= 1 then n else
  fib (n - 1) + fib (n - 2) in
print_int_ (fib 30);
print_char 10

(*
./main lib.ml; nasm out.s -f elf32 -g -o out.o; gcc -m32 out.o 
*)
