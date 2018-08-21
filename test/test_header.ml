let print_char x = print_char (Char.chr x)

(* from https://github.com/esumii/min-caml/blob/master/min-rt/miniMLRuntime.ml *)
let read_token () = 
	let rec read_token_rec ok acc =
		try 
		  let c = input_char stdin in
		  (match c with
		  | ' ' | '\t' | '\r' | '\n' -> (
		      if ok then acc
		      else read_token_rec false ""
				)
		  | _ -> read_token_rec true (acc ^ (Char.escaped c)))
		with
		  End_of_file -> if ok then acc else raise End_of_file
	in
		read_token_rec false ""

let read_int () = int_of_string (read_token ())

let read_float () = float_of_string (read_token ())

let fless x y = (x < y)

let create_array = Array.make

let fiszero x = if (x = 0.0) then 1 else 0

let fisneg x = if (x < 0.0) then 1 else 0

let fispos x = if (x > 0.0) then 1 else 0

let fhalf x = x /. 2.0

let fneg x = x *. (-1.0)

let fsqr x = x *. x

let fabs x = if x > 0.0 then x else (-.x)

let not x = if x = 0 then 1 else 0


;;
