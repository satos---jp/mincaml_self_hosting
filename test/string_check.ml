let s = "hello "
let t = "world"

;;
print_string (s ^ t);
print_char 10;
print_string s;
print_char 10;
print_string t;
print_char 10;
print_int (String.length s);
print_char 10;
print_int (Char.code (String.get s 3));
print_char 10

