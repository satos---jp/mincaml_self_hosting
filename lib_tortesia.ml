let false = 0

let rec fless a b = a < b

let rec fiszero x = (x = 0.0) 

let rec fisneg x = (x < 0.0)

let rec fispos x = (x > 0.0)

let rec floor x = float_of_int (int_of_float (if x < 0.0 then (x -. 1.0) else x))

let rec fabs x = if x > 0.0 then x else (-1.0 *. x)

let rec fsqr x = x *. x

let rec fneg x = (-1.0 *. x)

let rec fhalf x = x /. 2.0

