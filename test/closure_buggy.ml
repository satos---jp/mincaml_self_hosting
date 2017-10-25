(let rec f x = x + 123 in
let rec g y = f in
print_int ((g 456) 789));

(let rec f n =
  if n < 0 then () else
  (print_int n;
   let a = create_array 1 f in
   a.(0) (n - 1)) in
f 9);


(* Ž©—R•Ï”‚Ì‚ ‚éÄ‹AŠÖ” *)

(let x = 10 in
let rec f y =
  if y = 0 then 0 else
  x + f (y - 1) in
print_int (f 123));

(let rec h p = 
  let (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10) = p in
  let rec g z = 
    let r = v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 in
    if z > 0 then r else g (-z) in
  g 1 in 
print_int (h (1,2,3,4,5,6,7,8,9,10)))


