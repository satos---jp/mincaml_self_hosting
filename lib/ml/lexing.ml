(*
まず各々の正規表現をNFAにする。
あとはそれをデータとしてよしなに。
*)

open Nfa

type lexbuf = (unit -> char) * (int -> unit)

let from_channel ch = 
	if ch = stdin then (
		let rs = ref [] in
		let np = ref 0 in
		let f _ = 
			(if List.length (!rs) <= !np then
				let c = read_char () in
				(*
				print_string "read char";
				print_char 10;
				print_char c;
				print_char 10;
				*)
				rs := !rs @ [Char.chr c]
			else ());
			let p = !np in
			np := p + 1;
			let r = List.nth (!rs) p in
			(*
			print_string "return ";
			print_int r;
			print_char 10;
			*)
			r
		in
		let g t = 
			let rec sub i = 
				if i = 0 then ()
				else (rs := match !rs with x :: xs -> xs; sub (i-1))
			in
			 sub t;
			 np := 0
		in
			(f,g)
	) else (
		raise_match_failure "invalid number for from_channel"
	)

let getc buf = 
	let (g,_) = buf in g ()

let ungets buf t = 
	(*
	print_string "unget ";
	print_int t;
	print_char 10;
	*)
	let (_,h) = buf in h t

type rule_state = 
	| Going of state * nfa_compiled * ((string list * int) option) * (string list -> 'a)
	| Gone  of (string list * int) * (string list -> 'a)


let i2s i = Char.escaped (Char.chr (i+48))

let state2str st = 
	let rec f v = 
		match v with
		| [] -> ""
		| (d,_) :: ds -> (i2s d) ^ " " ^ (f ds)
	in
		f st

let stats2str ss = 
	"[" ^ (String.concat " ; " (List.map (fun x -> 
		match x with 
		| Gone(_,_) -> "Gone "
		| Going(st,_,_,_) -> "Going { " ^ (state2str st) ^ " } "
	) ss)) ^ "]"


(*
1. longest match
2. fist match
*)

let my_lexing buf data = 
	let rec f t c states = 
		let self = f t c in
		match states with
		| (Going(state,nfa,lasp,func)) :: xs -> (
				let ts = Nfa.step nfa state c in
				let tlasp = (
					if Nfa.isaccept nfa state then Some((Nfa.get_mem_from_state nfa state,t))
					else lasp
				) in
				
				if Nfa.isnill ts then
					match tlasp with
					| None -> self xs
					| Some p -> (Gone(p,func)) :: self xs
				else
					Going(ts,nfa,tlasp,func) :: self xs
			)
		| (Gone(a,b)) :: xs -> (Gone(a,b)) :: self xs
		| [] -> [] 
	in
	let rec circle t ss = 
		let c = getc buf in
		let tss = f t c ss in
		(*
		print_string "ciecle";
		print_char 10;
		print_string (stats2str tss);
		print_char 10;
		*)
		match tss with
		(*
		| [Gone(p,func)] -> (
				let (d,t) = p in
				ungets buf t;
				func d
			)
		*)
		| [] -> (
				print_string "lexing failed";
				print_char 10;
				print_string (stats2str tss);
				print_char 10;
				raise_match_failure "lexing failed"
			)
		(*　TODO(satos) このへん勢いで書いたのでわりとあやしそう *)
		| _ -> (
				if List.for_all (fun v -> match v with Gone(_,_) -> true | _ -> false) tss then (
					let longest = ref None in
					let _ = List.fold_left (fun ls x -> 
						match x with
						| Gone((d,t),func) -> (
								if t < ls then ls else (
									longest := Some x;
									t
								)
							)
					) (-1) (List.rev tss) in
					match !longest with
					| Some (Gone((d,t),func)) -> (
						ungets buf t;
						func d
					)
				) else (circle (t+1) tss)
			)
	in
	let starts = 
		List.map (fun (nfa,func,mem_ls) -> 
			let start_state = Nfa.nfa2start_state nfa mem_ls in
			Going(start_state,nfa,None,func)
		) data 
	in 
		circle 0 starts


(* TODO(satos) ちゃんとする *)
let new_line x = ()

