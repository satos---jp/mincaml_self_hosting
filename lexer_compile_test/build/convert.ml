open Syntax
open Nfa

let rec assoc_opt a v =
	match v with
	| [] -> None
	| (p,q) :: xs -> if a = p then Some q else assoc_opt a xs

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
	| CRange(c1,c2) -> (
			let a = Char.code c1 in
			let b = Char.code c2 in
			assert (a <= b);
			let rec f i = if i > b then [] else (Char.chr i) :: (f (i+1)) in f a
		) 

(* nfa g に、 st から gl まで をつなぐようにグラフを作る *)
let rec reg2trans env sym2i r g st gl = 
	let self = reg2trans env sym2i in
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
			let (ltg,lts) = List.fold_left (fun (tg,ts) nr -> 
				let (ttg,nts) = new_node tg in
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
				nfa_add_edge ng st gl (Char.code c)
			) g tcs
		)
	| RStar r -> (
			let tg = self r g gl st in
			nfa_add_epsilon_edge tg st gl
		)
	| RPlus r -> retry (RCons([r;RStar(r)]))
	| RId x -> retry (match assoc_opt x env with Some v -> v | None -> failwith (Printf.sprintf "id %s is undefined" x))
	| RAll -> retry (RCset([CRange('\x00','\xff')]))
	| REof -> nfa_add_edge g st gl eof_move
	| RAs(r,s) -> (
			let i = sym2i s in
			let (t1g,stk) = new_node g in
			let (t2g,glk) = new_node t1g in
			let t3g = self r t2g stk glk in
			let t4g = nfa_set_as t3g st stk i in
			nfa_unset_as t4g glk gl i
		)


let reg2nfa env sym2i r = 
	let init_nfa = gen_nfa () in
	reg2trans env sym2i r init_nfa 0 1

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
	let i2s = string_of_int in
	(Printf.sprintf "( %d , %s , %d , \n" st (list2str gl i2s) ma) ^
	list2str ts (fun (c,ss) -> 
		(Printf.sprintf "( %d ," c) ^ 
		list2str ss (fun (i,es) -> 
			(Printf.sprintf "( %d ," i) ^ 
			list2str es (fun (t,se,use) -> 
				(Printf.sprintf "( %d ," t) ^ 
				(list2str se  i2s) ^ "," ^
				(list2str use i2s) ^ ")"
			) ^ ")"
		) ^ ")"
	) ^ ")"

type var_type = 
	| VStr
	| VChar

let var_type2str v =
	match v with
	| VStr -> "VStr"
	| VChar -> "VChar"

let merge_var_type v1 v2 = 
	if v1 = v2 then v1 else 
	match v1,v2 with
	| VStr,_ | _,VStr -> VStr


let rec get_type_of_reg env r = 
	let self = get_type_of_reg env in
	match r with
	| RCset _ | RAll -> VChar
	| RStr _ -> VStr
	| RId na -> self (List.assoc na env)
	| ROr rs -> List.fold_left merge_var_type VChar (List.map self rs)
	| RCons rs -> (match rs with [r] -> self r | _ -> List.fold_left merge_var_type VStr (List.map self rs))
	| RStar r | RPlus r -> merge_var_type VStr (self r)

let gen_sym2i env reg = 
	let res = ref [] in
	let update_res na t = 
		match assoc_opt na (!res) with 
		| Some((i,v)) -> res := (na,(i,merge_var_type v t)) :: (List.remove_assoc na !res)
		| None -> let ls = List.length (!res) in res := (na,(ls,t)) :: !res
	in
	let rec f r = 
		match r with
		| RCset _ | RId _ | RStr _ | RAll | REof -> ()
		| RAs(nr,na) -> f nr; update_res na (get_type_of_reg env nr)
		| RStar r | RPlus r -> f r
		| ROr rs | RCons rs -> List.iter f rs
	in
	f reg;
	let rv = !res in
		((fun na -> let (i,_) = List.assoc na rv in i),rv)

let conv (decls,rules) =
	let bo = String.concat "\n" (List.mapi (fun gi (tag,args,ents) -> 
		let data = "[" ^ (String.concat ";\n" (List.map (fun (reg,co) -> 
			let (sym2i,syms) = gen_sym2i decls reg in 
			let s = nfa2str (reg2nfa decls sym2i reg) in
			let sym_let = 
				String.concat " " (List.map (fun (s,(i,st)) -> 
					(* Printf.printf "sym_let name: %s idx: %d type: %s\n" s i (var_type2str st); *)
					let sv = Printf.sprintf "(List.nth lexing_data %d)" i in
					match st with
					| VStr -> Printf.sprintf "let %s = %s in" s sv 
					| VChar -> Printf.sprintf "let %s = String.get %s 0 in" s sv 
				) syms)
			in
			(* "print_string \"length\"; print_int (List.length lexing_data); " ^ *)
			Printf.sprintf "(%s,(fun lexing_data -> %s (%s)),%d)" s sym_let co (List.length syms)
		) ents)) ^ "]" in
		(Printf.sprintf "%s %s %s lexbuf = \n" (if gi = 0 then "let rec" else "and") tag (String.concat " " args)) ^ 
		(Printf.sprintf "\tLexing.my_lexing lexbuf (%s)" data)
	) rules) in ("",bo)


