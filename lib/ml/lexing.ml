(*
まず各々の正規表現をNFAにする。
あとはそれをデータとしてよしなに。
*)

type lexbuf = unit -> char

let lexbuf_stdin _ = 
	let c = read_char () in
	print_string "read char";
	print_char 10;
	print_char c;
	print_char 10;
	c

let getc buf = 
	buf ()

type rule_state = 
	| Going of state * nfa * (unit option) * (unit -> 'a)
	| Gone  of unit * (unit -> 'a)

let my_lexing buf data = 
	let c = getc buf in
	let rec f states = 
		match states with
		| (Going(state,nfa,lasp,func)) :: xs -> (
				let ts = Nfa.step nfa state c in
				let tlasp = (
					if Nfa.isaccept nfa state then Some ()
					else lasp
				) in
				if isnill ts then
					match tlasp with
					| None -> f xs
					| Some p -> [Gone(p,func)]
				else
					Going(ts,nfa,tlasp,func) :: f xs
			)
		| (Gone(a,b)) :: _ -> [Gone(a,b)]
		| [] -> [] 
	in
	let rec circle ss = 
		let tss = f ss in
		match tss with
		| [Gone(p,func)] -> func p
		| [] -> raise_match_failure ()
		| _ -> circle tss 
	in
	let starts = 
		List.map (fun (nfa,func) -> Going([(0,[])],nfa,None,func)) data 
	in 
		circle starts

