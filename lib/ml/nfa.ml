type state = int list

type nfa = int

let rec assoc_opt y xs = 
	match xs with
	| (t,d) :: xs -> (
			if t = y then Some d else assoc_opt y xs
		)
	| [] -> None

let trans  (s,gr,gs) = gr
let fins   (s,gr,gs) = gs

	
let step gr (sts,ops) c = 
	let tops = List.fold_left (fun r s -> 
		let v = assoc_opt c ((trans gr) s) in
		match v with
		| Set ds -> (
				
				ds
			)
		| None -> []
	) ops sts) in

let isaccept x gr = List.mem x (fins gr)

let isnill st = (st = [])

let startstate (s,gr,gs) = s

let _ = 0
