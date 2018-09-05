let scale = create_array 24 0.0
let theta = create_array 24 0.0

let init_scale_theta = (
scale.(0) <- 1.0;
scale.(1) <- 0.5;
scale.(2) <- 0.25;
scale.(3) <- 0.125;
scale.(4) <- 0.0625;
scale.(5) <- 0.03125;
scale.(6) <- 0.015625;
scale.(7) <- 0.0078125;
scale.(8) <- 0.00390625;
scale.(9) <- 0.001953125;
scale.(10) <- 0.0009765625;
scale.(11) <- 0.00048828125;
scale.(12) <- 0.000244140625;
scale.(13) <- 0.0001220703125;
scale.(14) <- 0.00006103515625;
scale.(15) <- 0.000030517578125;
scale.(16) <- 0.0000152587890625;
scale.(17) <- 0.00000762939453125;
scale.(18) <- 0.000003814697265625;
scale.(19) <- 0.0000019073486328125;
scale.(20) <- 0.00000095367431640625;
scale.(21) <- 0.000000476837158203125;
scale.(22) <- 0.0000002384185791015625;
scale.(23) <- 0.00000011920928955078125;

theta.(0) <- 0.785398185253143310546875;
theta.(1) <- 0.463647603988647460937500;
theta.(2) <- 0.244978666305541992187500;
theta.(3) <- 0.124354995787143707275391;
theta.(4) <- 0.062418811023235321044922;
theta.(5) <- 0.031239833682775497436523;
theta.(6) <- 0.015623728744685649871826;
theta.(7) <- 0.007812341209501028060913;
theta.(8) <- 0.003906230209395289421082;
theta.(9) <- 0.001953122555278241634369;
theta.(10) <- 0.000976562208961695432663;
theta.(11) <- 0.000488281220896169543266;
theta.(12) <- 0.000244140625000000000000;
theta.(13) <- 0.000122070312500000000000;
theta.(14) <- 0.000061035156250000000000;
theta.(15) <- 0.000030517578125000000000;
theta.(16) <- 0.000015258789062500000000;
theta.(17) <- 0.000007629394531250000000;
theta.(18) <- 0.000003814697265625000000;
theta.(19) <- 0.000001907348632812500000;
theta.(20) <- 0.000000953674316406250000;
theta.(21) <- 0.000000476837158203125000;
theta.(22) <- 0.000000238418579101562500;
theta.(23) <- 0.000000119209289550781250)

let pi = 3.141592653589793238462643

let rec sin x =
   let rec internal_sin p q n angle negativeFlag =
      if n = 24 then
         (if negativeFlag then
            (fneg q)
         else
            q)
      else
         if angle >= 0.0 then
            internal_sin (p -. q *. scale.(n)) (q +. p *. scale.(n)) (n + 1) (angle -. theta.(n)) negativeFlag
         else
            internal_sin (p +. q *. scale.(n)) (q -. p *. scale.(n)) (n + 1) (angle +. theta.(n)) negativeFlag
   in
      let quotient = floor (x /. pi +. 0.5)
      in
         internal_sin 0.607252935008882777090378 0.0 0 (x -. quotient *. pi) (quotient -. floor (quotient /. 2.0) *. 2.0 = 1.0)


let rec cos x =
   let rec internal_cos p q n angle negativeFlag =
      if n = 24 then
         (if negativeFlag then
            (fneg p)
         else
            p)
      else
         if angle >= 0.0 then
            internal_cos (p -. q *. scale.(n)) (q +. p *. scale.(n)) (n + 1) (angle -. theta.(n)) negativeFlag
         else
            internal_cos (p +. q *. scale.(n)) (q -. p *. scale.(n)) (n + 1) (angle +. theta.(n)) negativeFlag
   in
      let quotient = floor (x /. pi +. 0.5)
      in
         internal_cos 0.607252935008882777090378 0.0 0 (x -. quotient *. pi) (quotient -. floor (quotient /. 2.0) *. 2.0 = 1.0)

let rec atan x =
let rec internal_atan p q n angle =
   if n = 24 then
      angle
   else
      if q < 0.0 then
         internal_atan (p -. q *. scale.(n)) (q +. p *. scale.(n)) (n + 1) (angle -. theta.(n))
      else
         internal_atan (p +. q *. scale.(n)) (q -. p *. scale.(n)) (n + 1) (angle +. theta.(n))
in
   internal_atan 1.0 x 0 0.0




let rec divten x = 
	let rec f l r x = 
		if l+1 >= r then l else  
		let m = (l+r) / 2 in
		if m * 10 <= x then f m r x else f l m x
	in
		if x = 0 then 0 else
		if x >= 0 then f 0 x x
		else (-(f 0 (-x) (-x)))

let rec print_int x = 
	let rec mod_ a b = a-(a/b)*b
	in
	let rec print_int_base x = 
		if x < 10 then (if 0 < x then print_char (x+48) else ()) else
			let dx = (divten x) in
			(print_int_base dx;
			print_char ((x-dx*10)+48))
	in
	if x = 0 then print_char 48
	else (if x < 0 then print_char 45; print_int_base (0-x) else print_int_base x)



