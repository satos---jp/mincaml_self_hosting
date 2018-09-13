let a = 1
let b = 1
let c = 3.2
let d = 3.2

;;
print_char (48 + (if a=b then 1 else 0));
print_char 10;
print_char (48 + (if c=d then 1 else 0));
print_char 10

let s = "hoge"
let t = "huga"
let u = "hogehuga"

;;
print_char (48 + (if s = t then 1 else 0));
print_char 10;
print_char (48 + (if s <> t then 1 else 0));
print_char 10;
print_char (48 + (if (s ^ t) = u then 1 else 0));
print_char 10;
print_char (48 + (if (s ^ t) <> u then 1 else 0));
print_char 10


let v = (a,s ^ t)
let w = (a,u)
let x = (a,s)

;;
print_char (48 + (if v = w then 1 else 0));
print_char 10;
print_char (48 + (if v <> w then 1 else 0));
print_char 10;
print_char (48 + (if v = x then 1 else 0));
print_char 10;
print_char (48 + (if v <> x then 1 else 0));
print_char 10


let xs = [1;2;3]
let ys = [4;5;6]
let zs = [1;2;3;4;5;6]
;;

print_char (48 + (if xs = ys then 1 else 0));
print_char 10;
print_char (48 + (if xs <> ys then 1 else 0));
print_char 10;
print_char (48 + (if (xs @ ys) = zs then 1 else 0));
print_char 10;
print_char (48 + (if (xs @ ys) <> zs then 1 else 0));
print_char 10


