type mem_state = 
	| Mem_Some of string
	| Mem_None
	| Mem_End of string

type state = (int * (mem_state list)) list
type node = int
type edge = int * int list * int list
(* to setlist unsetlist *)
type nfa = int * (int list) * int * (int * (node * edge list) list) list
(* start end集合 頂点数 trans *)
(* trans は、eof を -1 遷移にします。 *)
(* trans は、epsilon は -2 遷移 で。 *)

type nfa_compiled = int * (int list) * int * (int -> ((node * edge list) list) option)


let eof_move = -1
let eps_move = -2

let new_node (a,b,ml,v) = 
	((a,b,ml+1,v),ml)

let gen_nfa _ = (0,[1],2,[])


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

let nfa_add_edge (a,bs,ls,tr) st gl c = 
	let ne = (gl,[],[]) in
	let ttr = 
		update_list tr c (fun w -> 
			update_list w st (fun v -> ne :: v))
	in
		(a,bs,ls,ttr)


let nfa_add_epsilon_edge (a,bs,ls,tr) st gl = 
	nfa_add_edge (a,bs,ls,tr) st gl eps_move
		(* st からglの行き先に辺を張るに行くやつについて、全てに gl への辺を張る *)
		(* これではだめで、この後glから行く先に新たに辺がはられるとだめになる *)
	(*
	if st <> a then (
		let aes = ref [] in
		tr |> List.iter (fun (nc,ts) -> 
			ts |> List.iter (fun (fr,es) -> 
				es |> List.iter (fun (t,d1,d2) -> 
					if t = gl then aes := (nc,fr,d1,d2) :: !aes else ()
				)
			)
		);
		List.fold_left (fun g (nc,fr,d1,d2) -> 
			nfa_add_edge g fr gl nc
		) (a,bs,ls,tr) !aes
	) else (
		match [] with x :: xs -> (a,bs,ls,tr) 
	)
	*)


let nfa_set_unset_as isset (a,bs,ls,tr) st gl i = 
	let ne = if isset then (gl,[i],[]) else (gl,[],[i]) in
	let ttr = 
		update_list tr eps_move (fun w -> 
			update_list w st (fun v -> ne :: v))
	in
		(a,bs,ls,ttr)

let nfa_set_as = nfa_set_unset_as true
let nfa_unset_as = nfa_set_unset_as false


let state_idx (i,_) = i
let state_mems (_,m) = m

let rec collect_trans s fr_edge_list = 
	let i = state_idx s in
	match fr_edge_list with
	| [] -> []
	| (x,d) :: xs -> (
			let r = collect_trans s xs in
			if x = i then d @ r else r
		) 


let init_mems ids ms = 
	List.mapi (fun  i b -> 
		if List.mem i ids then Mem_Some "" else b
	) ms 


let update_mems c ms = 
	List.map (fun b -> 
		match b with
		| Mem_None -> b (* TODO(satos) この2行まとめて書きたいですね*)
		| Mem_End _ -> b
		| Mem_Some ns -> Mem_Some (ns ^ (Char.escaped c))
	) ms

let unset_mems ids ms = 
	List.mapi (fun i b -> 
		if List.mem i ids then (
			match b with 
			| Mem_Some ns -> Mem_End ns
		) else b
	) ms




let isaccept g st = 
	(*
	print_string ("isaccept check { " ^ (state2str st) ^ " } with " ^ (i2s (ends g)));
	print_string (Char.escaped (Char.chr 10));
	*)
	List.exists (fun v -> List.mem_assoc v st) (ends g)

let isnill st = (st = [])


let get_mem_from_state nfa st = 
	let nr = List.fold_left (fun r s -> 
		if isaccept nfa [s] then 
		state_mems s else r
	) [] st in
	List.map (fun d -> match d with Mem_End x -> x) nr

let edge_to (t,_,_) = t
let edge_inits (_,s,_) = s
let edge_unsets (_,_,s) = s

let i2s i = Char.escaped (Char.chr (i+48))
let state2str st = 
	let rec f v = 
		match v with
		| [] -> ""
		| (d,_) :: ds -> (i2s d) ^ " " ^ (f ds)
	in
		f st

(* TODO(satos) 書きながら設計ひどいなこれっつってるのでnfa.ml全体的にどうにかしたい *)
let rec saturate_step eps_trans rests s =
	let self = saturate_step eps_trans in
	if List.mem s rests then rests else (
		let ntr = collect_trans s eps_trans in
		let nms = state_mems s in
		List.fold_left (fun r e -> 
			let t = edge_to e in
			let tms = (
				nms |> 
				init_mems (edge_inits e) |> 
				unset_mems (edge_unsets e)
			) in
			self r (t,tms)
		) (s :: rests) ntr		
	)

let nfa2start_state g mem_ls = 
	let etrans = match (trans g) eps_move with Some v -> v | None -> [] in
	let rec f x = if x = 0 then [] else Mem_None :: f (x-1) in
	let s = (0,f mem_ls) in
	saturate_step etrans [] s 

let step g st c = 
	let fr_edge_tr_opt = (trans g) (Char.code c) in
	(*
	print_string ("step check { " ^ (state2str st) ^ " } with " ^ (Char.escaped c));
	print_string (Char.escaped (Char.chr 10));
	*)
	let etrans = match (trans g) eps_move with Some v -> v | None -> [] in
	match fr_edge_tr_opt with | None -> []
	| Some fr_edge_tr -> (
			List.fold_left (fun r s -> 
				let es = collect_trans s fr_edge_tr in
				let nms = state_mems s in
				List.fold_left (fun r e -> 
					let t = edge_to e in
					let tms = (
						nms |> 
						init_mems (edge_inits e) |> 
						update_mems c |> 
						unset_mems (edge_unsets e)
					) in
					saturate_step etrans r (t,tms)
				) r es
			) [] st
		)


