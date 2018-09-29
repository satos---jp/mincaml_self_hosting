type state = (int * (string list)) list
type node = int
type edge = int * int list * int list
type nfa = int * int * int * (char * (node * edge list) list) list

let new_node (a,b,ml,v) = 
	((a,b,ml+1,v),ml)

let gen_nfa _ = (0,1,2,[])


let rec assoc_opt y xs = 
	match xs with
	| (t,d) :: xs -> (
			if t = y then Some d else assoc_opt y xs
		)
	| [] -> None

let trans (_,_,_,ts) = ts
let ends (_,x,_,_) = x

let rec unique v = 
	match v with
	| [] -> []
	| x :: xs -> (
		let txs = unique xs in
			if List.mem x txs then txs else x :: txs
		)

(*
state :: (idx,mem) の list

mem :: (buffer,str) の idx 個分のlist

trans :: [(char,[(idx,[辺集合])])]

edge :: to,[set],[unset]
*)


let update_list v i f = 
	if List.mem_assoc i v then
		List.map (fun (x,d) -> (x,if x = i then f d else d)) v
	else
		(i,f []) :: v


(* TODO(satos) そろそろ真面目にrank1多相しないといけない... *)
let update_list_2 v i f = 
	if List.mem_assoc i v then
		List.map (fun (x,d) -> (x,if x = i then f d else d)) v
	else
		(i,f []) :: v

let nfa_add_edge (a,b,ls,tr) st gl c = 
	let ne = (gl,[],[]) in
	let ttr = 
		update_list tr c (fun w -> 
			update_list_2 w st (fun v -> ne :: v))
	in
		(a,b,ls,ttr)



let state_idx (i,_) = i
let state_mems (_,m) = m

let rec collect_trans s ts = 
	let i = state_idx s in
	match ts with
	| [] -> []
	| (x,d) :: xs -> (
			let r = collect_trans s xs in
			if x = i then d @ r else r
		) 

let init_mems ids ms = 
	List.mapi (fun  i (b,s) -> 
		if List.mem i ids then (Some "",s) else (b,s)
	) ms 

let update_mems c ms = 
	List.map (fun (b,s) -> 
		match b with
		| None -> (b,s)
		| Some ns -> (Some (ns ^ c),s)
	) ms

let unset_mems ids ms = 
	List.mapi (fun i (b,s) -> 
		if List.mem i ids then (
			match b with 
			| Some ns -> (None,ns)
		) else (b,s)
	) ms 

(* TODO(後で消す) *)
let rec map2 f v = 
	match v with
	| x :: xs -> (f x) :: (map2 f xs)
	| [] -> []

let edge_to (t,_,_) = t
let edge_inits (_,s,_) = s
let edge_unsets (_,_,s) = s

let step g sts c = 
	let gts = assoc_opt c (trans g) in
	match gts with | None -> []
	| Some ts -> (
		unique (List.concat (List.map (fun s -> 
			let trs = collect_trans s ts in
			let nms = state_mems s in
			map2 (fun e -> 
				let t = edge_to e in
				let tms = (
					nms |> 
					init_mems (edge_inits e) |> 
					update_mems c |> 
					unset_mems (edge_unsets e)
				) in
				(t,tms)
			) trs
		) sts))
	)

let i2s i = Char.escaped (Char.chr (i+48))
let state2str st = 
	let rec f v = 
		match v with
		| [] -> ""
		| (d,_) :: ds -> (i2s d) ^ " " ^ (f ds)
	in
		f st

let isaccept g st = 
	print_string ("{ " ^ (state2str st) ^ " }");
	print_char 10;
	List.mem_assoc (ends g) st

let isnill st = (st = [])


