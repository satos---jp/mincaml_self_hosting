open Syntax


(*
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

*)


(* PType... -> mliを作るためにいるっぽい(parserはmliを作れる!!) *)

let conv_prec pcs = 
	let sv = (0,"","") in
	List.fold_left (fun ((idn,type_token_s,token2id_s) as r) pc -> 
		match pc with
		| PToken s -> (
				idn+1,
				type_token_s ^ (Printf.sprintf "| %s\n" s),
				token2id_s   ^ (Printf.sprintf "| %s -> %d\n" s idn)
			)
		| PTToken(et,s) -> (
				idn+1,
				type_token_s ^ (Printf.sprintf "| %s of %s\n" s et),
				token2id_s   ^ (Printf.sprintf "| %s _ -> %d\n" s idn)
			)
		| PStart _ | PType _  -> r
	) sv pcs

let rec unique v = 
	match v with
	| [] -> []
	| x :: xs -> (
			let tv = unique xs in
			if List.mem x tv then tv else x :: tv
		)


let sym2funname sm = "fold_" ^ sm

let extract_folds_from_precs pcs = 
	(String.concat "" (List.map (fun pc -> 
		match pc with
		| PTToken(et,s) -> (
				(Printf.sprintf "let %s d =\n" (sym2funname s)) ^
				"\tmatch d with\n" ^
				"\t| Token t -> t\n\n"
			)
		| _ -> ""
	) pcs))

let extract_folds rules = 
	"let rec " ^ (String.concat "\n\nand " (List.map (fun (na,rls) ->  
		(Printf.sprintf "%s d =\n" (sym2funname na)) ^
		"\tmatch d with\n" ^
		"\t| Datum(_,i,ds) -> (\n" ^
		"\t\t\t" ^ (String.concat "" (List.mapi (fun i (syms,(co,ds)) -> 
			let uds = unique ds in
			(Printf.sprintf "if i = %d then (\n" i) ^
			(String.concat "" (List.map (fun p -> 			
				(* Printf.printf "%d of %d\n" p (List.length syms); *)
				let sm = List.nth syms (p-1) in
				(Printf.sprintf "\t\t\t\tlet _%d = %s (List.nth ds %d) in\n" p (sym2funname sm) p)
			) uds)) ^
			"\t\t\t\t\t(" ^ co ^ ")\n" ^
			"\t\t\t) else "
		) rls)) ^ 
		(Printf.sprintf "(raise_match_failure \"parser %s failed\")\n" na) ^
		"\t\t)"
	) rules)) ^
	"\n"
	
let conv (header,precs,rules) =
	let _,type_token_s,token2id_s = conv_prec precs in
	header ^ 
	"type token = \n" ^ type_token_s ^ 
	"let token2id d = match d with\n" ^ token2id_s ^ "\n" ^
	(extract_folds_from_precs precs) ^
	(extract_folds rules)




