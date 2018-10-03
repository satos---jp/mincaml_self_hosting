"""	
print_char ((let rec f x = "hoge" in f) 10)

"""

"""
let v = create_array 1  (fun x -> x) in
v.(0) <- (fun x -> let (p,q) = x in (p+3,q)); 
let g = v.(0) in
let x = g 6 in
print_char 66;
print_char 10

let v = ref (fun x -> x) in
v := (fun x -> let (p,q) = x in (p+3,q)); 
let g = !v;
g 6


# let p = List.map (fun x -> x);;
val p : '_a list -> '_a list = <fun>
# p [1;2;3];;
- : int list = [1; 2; 3]
# p [true];;
Error: This expression has type bool but an expression was expected of type
         int

なので、普通に値多相制限入れれば良さそう


# let g = fun x -> x in g;;
- : 'a -> 'a = <fun>
# let g = match (fun x -> x) with f -> f;;
val g : 'a -> 'a = <fun>
# (fun x -> x) (fun x -> x);;
- : '_a -> '_a = <fun>
# List.map (fun x -> x);;
- : '_a list -> '_a list = <fun>
# (fun x -> x) [];;
- : 'a list = []

"""

