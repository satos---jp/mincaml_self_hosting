(*
まず各々の正規表現をNFAにする。
あとはそれをデータとしてよしなに。
*)

type lexbuf = unit -> char

let lexbuf_stdin _ = 
	let c = read_char () in
	(*
	print_string "read char";
	print_char 10;
	print_char c;
	print_char 10;
	*)
	Char.chr c

let getc buf = 
	buf ()

type rule_state = 
	| Going of state * nfa * (unit option) * (unit -> 'a)
	| Gone  of unit * (unit -> 'a)


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


let my_lexing buf data = 
	let rec f c states = 
		match states with
		| (Going(state,nfa,lasp,func)) :: xs -> (
				let ts = Nfa.step nfa state c in
				let tlasp = (
					if Nfa.isaccept nfa state then Some ()
					else lasp
				) in
				(*
				print_string (if Nfa.isaccept nfa state then "T" else "F");
				print_char 32;
				print_string (match tlasp with Some _ -> "Some" | None -> "None");
				print_char 10;
				*)
				if Nfa.isnill ts then
					match tlasp with
					| None -> f c xs
					| Some p -> [Gone(p,func)]
				else
					Going(ts,nfa,tlasp,func) :: f c xs
			)
		| (Gone(a,b)) :: _ -> [Gone(a,b)]
		| [] -> [] 
	in
	let rec circle ss = 
		let c = getc buf in
		(*
		print_string "ciecle";
		print_char 10;
		print_string (stats2str ss);
		print_char 10;
		*)
		let tss = f c ss in
		match tss with
		| [Gone(p,func)] -> func p
		| [] -> (
				print_string "lexing failed";
				print_char 10;
				print_string (stats2str tss);
				print_char 10;
				raise_match_failure ()
			)
		| _ -> circle tss 
	in
	let starts = 
		List.map (fun (nfa,func) -> Going([(0,[])],nfa,None,func)) data 
	in 
		circle starts

