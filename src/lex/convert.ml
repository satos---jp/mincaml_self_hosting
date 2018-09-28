open Syntax
open Nfa


let str2cs s = 
	let ls = String.length s in
	let rec f i = 
		if i >= ls then []
		else (String.get s i) :: f (i+1)
	in
		f 0



let cset2cs se = 
	match se with
	| CChar c -> [c]

let rec reg2trans env r g st gl = 
	let self = reg2trans env in
	let retry tr = self tr g st gl in
	match r with
	| ROr rs -> (
			List.fold_left (fun tg nr -> 
				self nr tg st gl
			) g rs
		)
	| RCons rs -> (
			let hr = List.hd rs in
			let trs = List.tl rs in
			let ltg,lts = List.fold_left (fun (tg,ts) nr -> 
				let ttg,nts = new_node tg in
				let rg = self nr ttg nts ts in
					(rg,nts)
			) (g,gl) (List.rev trs) in
			self hr ltg st lts
		)
	| RStr s -> (
			let tr = RCons(List.map (fun c -> RCset([CChar c])) (str2cs s)) in 
				retry tr
		)
	| RCset cs -> (
			let tcs = List.concat (List.map cset2cs cs) in
			List.fold_left (fun ng c ->
				nfa_add_edge ng st gl c
			) g tcs
		)



let reg2nfa env r = 
	let init_nfa = gen_nfa () in
	reg2trans env r init_nfa 0 1

(* TODO(satos) nfa2strをlib/ml/nfa.ml に戻す *)


let list2str v f =
	"[" ^ (
		let rec self w = 
			match w with
			| [] -> "]"
			| [x] -> (f x) ^ "]"
			| x :: xs -> (f x) ^ ";" ^ (self xs)
		in 
			self v
	)

let nfa2str (st,gl,ma,ts) = 
	(Printf.sprintf "( %d , %d , %d , \n" st gl ma) ^
	list2str ts (fun (c,ss) -> 
		(Printf.sprintf "( \'%c\' ," c) ^ 
		list2str ss (fun (i,es) -> 
			(Printf.sprintf "( %d ," i) ^ 
			list2str es (fun (t,se,use) -> 
				(Printf.sprintf "( %d ," t) ^ 
				(list2str se  (Printf.sprintf "%d")) ^ "," ^
				(list2str use (Printf.sprintf "%d")) ^ ")"
			) ^ ")"
		) ^ ")"
	) ^ ")"


let conv (decls,rules) =
	let bo = String.concat "" (List.map (fun (tag,args,ents) -> 
		let data = "[" ^ (String.concat ";\n" (List.map (fun (reg,co) -> 
			let s = nfa2str (reg2nfa decls reg) in
			Printf.sprintf "(%s,fun _ -> (%s))" s co (* TODO(satos) かっこにする *)
		) ents)) ^ "]" in
		(Printf.sprintf "let rec %s %s lexbuf = \n" tag (String.concat " " args)) ^ 
		(Printf.sprintf "\tLexing.my_lexing lexbuf (%s)" data)
	) rules) in ("",bo)


