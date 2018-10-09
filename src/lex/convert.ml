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
				nfa_add_edge ng st gl (Char.code c)
			) g tcs
		)
	| RStar r -> (
			let tg = self r g gl st in
			nfa_add_epsilon_edge tg st gl
		)
	| RPlus r -> retry (RCons([r;RStar(r)]))
	| RId x -> retry (List.assoc x env)
	| RAll -> retry (RCset([CRange('\x00','\xff')]))
	| RAs(r,s) -> (
			let i = sym2i s in
			let t1g,stk = new_node g in
			let t2g,glk = new_node t1g in
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


let gen_sym2i () = 
	let c = ref [] in
	(fun x -> try List.assoc x !c with Not_found -> let ls = List.length !c in c := (x,ls) :: !c; ls),
	c

let conv (decls,rules) =
	let bo = String.concat "" (List.map (fun (tag,args,ents) -> 
		let data = "[" ^ (String.concat ";\n" (List.map (fun (reg,co) -> 
			let sym2i,syms = gen_sym2i () in 
			let s = nfa2str (reg2nfa decls sym2i reg) in
			let sym_let = 
				String.concat " " (List.map (fun (s,i) -> 
					Printf.sprintf "let %s = List.nth lexing_data %d in " s i
				) !syms)
			in
			(* "print_string \"length\"; print_int (List.length lexing_data); " ^ *)
			Printf.sprintf "(%s,(fun lexing_data -> %s (%s)),%d)" s sym_let co (List.length !syms)
		) ents)) ^ "]" in
		(Printf.sprintf "let rec %s %s lexbuf = \n" tag (String.concat " " args)) ^ 
		(Printf.sprintf "\tLexing.my_lexing lexbuf (%s)" data)
	) rules) in ("",bo)


