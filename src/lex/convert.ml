open Syntax
open Nfa

(*
まず各々の正規表現をNFAにする。
あとはそれをデータとしてよしなに。

let main v1 v2 ... lexbuf = 
	let c = getc lexbuf in
	let rec f states = 
		match states with
		| Going(state,nfa,lastp,func) :: xs -> (
				let ts = step nfa state c in
				let tlasp = (
					if isaccept nfa state then
					()
					else lastp
				) in
				if isnill ts then
					match lastp with
					| None -> f xs
					| Some p -> Gone(p,func)
				else
					Going(ts,nfa,lastp) :: f xs
			)
		| Gone(p) :: _ -> [Gone(p)]
		| [] -> []
	in
	let rec circle ss = 
		let tss = f ss in
		match tss with
		| [Gone(x)] -> x
		| [] -> raise failure
		| _ -> circle tss 
	in circle (startstate nfa)

みたいなやつ。
*)




let conv (decls,rules) =
	List.map (fun (tag,args,ents) -> 
		let data = gen_automaton decls ents in
		()
	) rules
