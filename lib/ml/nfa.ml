
(*
type state = int list

let rec assoc_opt y xs = 
	match xs with
	| (t,d) :: xs -> (
			if t = y then Some d else assoc_opt y xs
		)
	| [] -> None

let trans  (s,gr,gs) = gr
let fins   (s,gr,gs) = gs
	
let step gr st c = 
	List.concat (List.map (fun s -> 
		let v = assoc_opt c ((trans gr) s) in
		match v with
		| Some ds -> ds
		| None -> []
	) st)
	
let isaccept x gr = List.mem x (fins gr)

let isnill st = (st = [])

let startstate (s,gr,gs) = s
*)

let _ = 0
