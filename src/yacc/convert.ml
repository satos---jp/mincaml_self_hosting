open Syntax

(* PType... -> mliを作るためにいるっぽい(parserはmliを作れる!!) *)

let collect_start pcs = 
	let ns = List.fold_left (fun v pc -> 
		match pc with
		| PStart x -> x :: v
		| _ -> v
	) [] pcs
	in
	let ts = List.fold_left (fun v pc -> 
		match pc with
		| PType(et,s) -> (s,et) :: v
		| _ -> v
	) [] pcs
	in
	List.map (fun s -> 
		try 
			(s,List.assoc s ts)
		with | Not_found -> 
			failwith ("start symbol " ^ s ^ " should have type declation")
	) ns

let conv_prec pcs = 
	let sv = (0,"","",[]) in
	List.fold_left (fun ((idn,type_token_s,token2id_s,tokens) as r) pc -> 
		match pc with
		| PToken s -> (
				idn+1,
				type_token_s ^ (Printf.sprintf "| %s\n" s),
				token2id_s   ^ (Printf.sprintf "| %s -> %d\n" s idn),
				(s,0) :: tokens
			)
		| PTToken(et,s) -> (
				idn+1,
				type_token_s ^ (Printf.sprintf "| %s of %s\n" s et),
				token2id_s   ^ (Printf.sprintf "| %s _ -> %d\n" s idn),
				(s,1) :: tokens
			)
		| PStart _ | PType _ | PLeft _ | PRight _ | PNonassoc _ -> r 
	) sv pcs



type precedence_tag = 
	| Left | Right | Nonassoc


let symbol2precedence sym precs = 
	try
		Some(List.assoc sym precs)
	with
		| Not_found -> None

let precedence2str pr = 
	match pr with
	| Some((i,t)) -> (
			Printf.sprintf "(%d,%s)" i 
			(match t with Left -> "Left" | Right -> "Right" | Nonassoc -> "Nonassoc") 
		)
	| None -> "None"

let get_precedence_order pr ps = 
	(*
		Printf.printf "compare %s v.s. %s\n" (precedence2str pr) (precedence2str ps);
		flush_all ();
	*)
	match pr,ps with
	| Some((ir,it)),Some((sr,st)) -> (
			if ir > sr then 1 
			else if ir < sr then -1 
			else (
				assert (it = st);
				match it with
				| Left -> 1
				| Right -> -1
				| Nonassoc -> 0
			)
		)
	| _ -> 0

let collect_precedence pcs = 
	let sv = 0,[] in
	let _,res = List.fold_left (fun ((i,v) as r) pc -> 
		match pc with
		| PLeft ss -> (i+1,(List.map (fun s -> (s,(i,Left))) ss) @ v)
		| PRight ss -> (i+1,(List.map (fun s -> (s,(i,Right))) ss) @ v)
		| PNonassoc ss -> (i+1,(List.map (fun s -> (s,(i,Nonassoc))) ss) @ v)
		| PStart _ | PType _ | PToken _ | PTToken _ -> r 
	) sv pcs
	in 
		res


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
				(Printf.sprintf "\t| Token t -> match t with %s(x) -> x\n\n" s)
			)
		| _ -> ""
	) pcs))

let extract_folds rules = 
	"let rec " ^ (String.concat "\n\nand " (List.map (fun (na,rls) ->  
		(Printf.sprintf "%s d =\n" (sym2funname na)) ^
		(*
		(Printf.sprintf "\tprint_string \"%s\"; print_char 10;\n" (sym2funname na)) ^
		*)
		"\tmatch d with\n" ^
		"\t| Datum(k,i,ds) -> (\n" ^
		(*
		"\tprint_int k; print_char 32; print_int i; print_char 10;\n" ^
		*)
		"\t\t\t" ^ (String.concat "" (List.mapi (fun i (syms,(_,(co,ds))) -> 
			let uds = unique ds in
			(Printf.sprintf "if i = %d then (\n" i) ^
			(String.concat "" (List.map (fun p -> 			
				(* Printf.printf "%d of %d\n" p (List.length syms); *)
				let sm = List.nth syms (p-1) in
				(Printf.sprintf "\t\t\t\tlet _%d = %s (List.nth ds %d) in\n" p (sym2funname sm) (p-1))
			) uds)) ^
			"\t\t\t\t\t(" ^ co ^ ")\n" ^
			"\t\t\t) else "
		) rls)) ^ 
		(Printf.sprintf "(raise_match_failure \"parser %s failed\")\n" na) ^
		"\t\t)"
	) rules)) ^
	"\n"

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




let find_idx a v = 
	let rec f v i = 
		match v with
		| [] -> None
		| x :: xs -> if x = a then Some i else f xs (i+1)
	in f v 0
(*
let vec_append v d = v := d :: !v
*)

let assoc_idx a v = 
	let rec f v i = 
		match v with
		| [] -> None
		| (x,d) :: xs -> if x = a then Some((i,d)) else f xs (i+1)
	in f v 0
	
let enumerate v = List.mapi (fun i x -> (x,i)) v



let rec assoc_update (x,a) v = 
	match v with
	| [] -> [(x,a)]
	| (y,b) :: xs -> if x = y then (x,a) :: xs else (y,b) :: (assoc_update (x,a) xs)

(*
let resum2str v = 
	match v with
	| Reduce_data(vs,i,j) -> Printf.sprintf "Reduce{%s,%d,%d}" (list2str vs string_of_int) i j
	| Shift_data(i,j) -> Printf.sprintf "Shift{%d,%d}" i j
*)

let rec update_nth v i d = 
	match v with
	| x :: xs -> if i = 0 then d :: xs else x :: (update_nth xs (i-1) d)


(* vからappendして作ったようなものもアップデートされるようにしたい *)
(* すなわち、
	let v = [1;2]
	let w = append_fs k v;
	append_fs v x;
	のとき、
	wにx分が増えてほしい
*)

(*
vが上書きされる
wは上書きされない
wが上書きされたらvも上書きされる
変則unionfind的なやつですね
*)

type 'a union_node = 
	| Node of ((int list) ref)
	| Data of 'a list

let union = ref []

let append_fs v w =
	let ls = List.length !union in
	if v = w then () else (
		match List.nth !union (ls-v-1) with
		| Node ds -> (
				if List.mem w !ds then () else (
					ds := w :: !ds
				)
			)
	)


let rec range a b = 
	if a < b then a :: (range (a+1) b) else []

let unit_fs xs = 
	(* xの先頭にありうるような終端記号すべてを追加する *)
	let i = List.length !union in
	union := (Data(xs)) :: !union;
	union := (Node(ref [i])) :: !union;
	(i+1)

let copy_from_fs x = 
	let i = List.length !union in
	union := (Node(ref [x])) :: !union;
	i

let fs2list i =
	let ls = List.length !union in
	let gone = ref [] in
	let rec f i = 
		if List.mem i !gone then [] else (
			gone := i :: !gone;
			(* Printf.printf "get %d\n" i; *)
			let v = List.nth !union (ls-i-1) in
			match v with
			| Data x -> x
			| Node ds -> unique (List.concat (List.map f !ds))
		)
	in
		f i


type parsingact_before = 
	| Shift_data of int * int (* 入力として記号iが来たら状態jに遷移する *)
	| Reduce_data of int * int * int (* 入力記号がvs中のとき、シンボルiのj番めのルールでreduceする *)

type rl_item = ((int * int) * (int list) * (int list)) * ((int list) ref) (* symbol/rule/dotより前/dotより後/follow集合 *)


let merge_state v w = 
	List.iter (fun (k,av) -> 
		let v = List.assoc k v in
		append_fs v av
	) w

type parsingact = 
	| Shift of int
	| Reduce of int * int
	| Error

let parsingact2str pa = 
	match pa with
	| Shift(x) -> Printf.sprintf "Parsing.Shift(%d)" x
	| Reduce(x,y) -> Printf.sprintf "Parsing.Reduce(%d,%d)" x y
	| Error -> Printf.sprintf "Error"



(* (((i,j),ss,ts),fs) *)
let get_hash_of_state st = 
	List.sort (fun (a,b,c) (d,e,f) -> 
		if (a,b,c) = (d,e,f) then 0 
		else if (if a <> b then a < b else if c <> d then c < d else e < f) then 1
		else -1)
		(List.map (fun (((i,j),ss,ts),fs) -> (i,j,List.length ss)) st)
	

		

(* table :: 状態stでi番めのデータが降ってきた際にshift/reduceするやつ *)
(* table は、大きさを縮めるために 各 st にたいして (i,move) の組とする (型は (int * parsingact) list )*)
let make_table (rules : rule list) sym2i i2sym i_sym_data startsym precedences = 
	Printf.printf "make_table for start %s\n" startsym; flush stdout;
	let parsingact2readablestr pa = 
		match pa with
		| Shift(x) -> Printf.sprintf "Shift[%d]" x
		| Reduce(x,y) -> Printf.sprintf "Reduce[%s,%d]" (i2sym x) y
		| Error -> Printf.sprintf "Error"
	in

	(* symbol x が先頭となるようなセットを返す *)
	let firstset x = 
		let res = ref [] in
		let gone = ref [] in
		let rec f nv = 
			if List.mem nv !gone then () else (
				gone := nv :: !gone;
				try 
					let nrs = List.assoc (i2sym nv) rules in
					List.iter (fun (sls,_) -> 
						match sls with
						| a :: _ -> f (sym2i a)
						| _ -> failwith ("Null rule is prohibited (at " ^ (i2sym nv) ^ ")")
					) nrs
				with
					| Not_found -> res := nv :: !res
			)
		in
		f x;
		!res 
	in
	(*
	let memorize f = 
		let mem = ref [] in
		let tf x = 
			try 
				List.assoc x !mem
			with
				| Not_found -> (
						let r = f x in
						mem := (x,r) :: !mem;
						r
					)
		in
			tf
	in
	let firstset = memorize firstset in
	*)
	
	let get_precedence pa = 
		match pa with
		| Reduce(x,y) -> (
				let ss,(p,_) = List.nth ((List.nth rules x) |> snd) y in
				match p with
				| Some x -> symbol2precedence x precedences
				| None -> (
					List.fold_right (fun x r -> 
						match r with 
						| None -> symbol2precedence x precedences
						| Some _ -> r
					) ss None
				)
			)
	in
	let rl2str (((i,j),ss,ts),fs) = (
	(*
		(Printf.sprintf "{(%d,%d),%s,%s} " i j (list2str ss string_of_int) (list2str ts string_of_int)) ^
	*)
		(Printf.sprintf "{(%s,%d),%s,%s:%s}" (i2sym i) j (list2str ss i2sym) (list2str ts i2sym) (list2str (fs2list fs) i2sym))
	)
	in
	let irules = List.map (fun (s,d) -> 
		(* Printf.printf "rule %s idx %d\n" s (sym2i s); *) 
		(sym2i s,(d |> (List.map fst) |> (List.map (fun x -> List.map sym2i x)) |> enumerate))
	) rules in
	
	(* 0.1sec はさすがに遅いよなぁ... *)
	let rec saturate acc ds = 
		(* Printf.printf "%s\n" (rl2str d); *)
		let self = saturate in
		match ds with
		| [] -> acc
		| d :: rds -> (
				let v = self acc rds in
				try 
					(* ここでLALR(1)にする *)
					let kd,dv = d in
					let otf = List.assoc kd v in
					append_fs otf dv;
					v
				with
					| Not_found -> (
						match d with
						| (_,_,[]),_ -> d :: v
						| (_,_,x :: xs),fo -> (
							(* Printf.printf "search by %d\n" x; *)
							let nru = try List.assoc x irules with Not_found -> [] in
							let tfo = match xs with [] -> (copy_from_fs fo) | p :: _ -> unit_fs (firstset p) in
(*
                                                       List.fold_left (fun r (isyms,j) -> 
                                                               self r [((x,j),[],isyms),tfo]
                                                       ) (d :: v) nru
	*)						
							self (d :: v) (List.rev (List.map (fun (isyms,j) -> (((x,j),[],isyms),tfo)) nru))
	
						)
					)
			)
	in
	let saturate = saturate [] in
	
	Printf.printf "init vs es\n"; flush stdout;
	let vs = ref [] in
	let es = ref [] in
	let vses2str () = 
		(List.combine !vs !es) |> 
		(List.mapi (fun i ((_,v),e) ->
			(Printf.sprintf "state %d:\n\t" i) ^ 
			(v |>
			(List.filter (fun ((_,be,_),_) -> be <> [])) |> 
			(List.map rl2str) |>
			String.concat "\n\t") ^ 
			"\nwith edge\n" ^
			(list2str (!e) (fun d -> 
				match d with
				| Reduce_data(vs,i,j) -> Printf.sprintf "Reduce[%s,%s,%d]" (list2str (fs2list vs) i2sym) (i2sym i) j
				| Shift_data(i,j) -> Printf.sprintf "Shift[%s,%d]" (i2sym i) j
			)) ^ "\n"
		)) |>
		(String.concat "")
  in
	let rec add_state_node it = 
		let self = add_state_node in
		Printf.printf "before saturate\n";
		flush stdout;
		let tit = saturate it in
		Printf.printf "vs.size %d es.size %d\n" (List.length !vs) (List.length !es);
		(*
		print_string "its ::\n";
		List.iter (fun d -> 
			print_string ("add node " ^ (rl2str d) ^ "\n")
		) it;	
		print_string "tits ::\n";
		List.iter (fun d -> 
			print_string ("add node " ^ (rl2str d) ^ "\n")
		) tit;
		*)
		flush stdout;
		
		let tit_hash = get_hash_of_state tit in
		match assoc_idx tit_hash (!vs) with
		| Some((t,d)) -> (
				merge_state d tit;
				t
			)
		| None -> (
				(*
				Printf.printf "%s\n" (vses2str ());
				print_string "========================================\n";
				*)
				let rt = List.length !vs in
				vs := (!vs) @ [tit_hash,tit];
				let ne = ref [] in
				es := !es @ [ne];
				(* 終了していて、reduceするアイテム *)
				ne := List.concat (List.map (fun (((p,q),be,af),fo) -> 
					match af with
					| [] -> [Reduce_data(fo,p,q)]
					| _ -> []
				) tit);
				
				(* 終了していなくて、shiftするアイテム *)
				let samexs = ref [] in
				List.iter (fun (((p,q),be,af),fo) -> 
					match af with
					| [] -> ()
					| x :: xs -> (
							let d = (((p,q),be @ [x],xs),fo) in
							try 
								let v = List.assoc x !samexs in
								v := d :: !v
							with
								| Not_found -> samexs := (x,ref [d]) :: !samexs
						)
				) tit;
				ne := !ne @ (List.map (fun (x,ds) -> let nt = self !ds in Shift_data(x,nt)) !samexs);
				rt
			)
	in
	let stp = add_state_node [(((sym2i "@start_start"),-1),[],[sym2i startsym]),(unit_fs [(sym2i "@end_end")])] in
	
	(* let v = x :: xs in List.nth (List.length xs) が x と違うの罠過ぎませんか??? *)
	
	let ds = vses2str () in
	Printf.printf "%s\n" ds;
	flush stdout;
	
	list2str (List.mapi (fun i e -> (i,e)) !es) (fun (idx,e) -> 
		Printf.printf "at node %d\n" idx;
		flush stdout;
		let te = (let rec f i = if i = 0 then [] else (ref Error) :: (f (i-1)) in f (List.length !i_sym_data)) in
		let te_update i v = 
			let d = List.nth te i in
			match !d with
			| Error -> d := v
			| Shift _ | Reduce _ -> (
				let errstr = (
					Printf.sprintf 
						"conflict state %d token %s | %s v.s. %s" 
						idx (i2sym i) (parsingact2readablestr !d) (parsingact2readablestr v)
				) in
				match !d,v with
				| Reduce _,Reduce _ -> failwith ("reduce/reduce " ^ errstr)
				| _ -> (
					let tv = ref v in
					(match !d,v with
					| Shift _,Reduce _ -> tv := !d; d := v
					| Reduce _,Shift _  -> ());
					let pd = get_precedence !d in
					let pv = symbol2precedence (i2sym i) precedences in
					let ord = get_precedence_order pd pv in
					(if ord < 0 then (
						d := !tv
					) else if ord > 0 then (
					) else (
						failwith ("shift/reduce " ^ errstr)
					));
					Printf.printf "compare %s and choose %s\n" errstr (parsingact2readablestr !d)
				)
			)
		in
		List.iter (fun d -> 
			match d with
			| Shift_data(i,j) -> te_update i (Shift(j))
			| Reduce_data(vs,j,k) -> (
					if k < 0 then () else (
						List.iter (fun i -> te_update i (Reduce(j,k))) (fs2list vs)
					)
				)
		) !e;
		(Printf.sprintf "(* %d *) " idx) ^
		(list2str (te |> (List.mapi (fun i d -> (i,d))) |> (List.filter (fun (_,d) -> !d <> Error))) (fun (i,d) -> 
			Printf.sprintf "(%d,%s)" (if i = (sym2i "@end_end") then -1 else i) (parsingact2str !d)
		)) ^ "\n"
	)



(*
let get_rule2idx rules = 
	let ls = ref [] in
	let v = List.fold_left (fun r (na,ds) -> 
		let t = List.length !ls in
		List.iter (fun (d,_) -> ls := !ls @ [List.length d]) ds;
		(na,(List.mapi (fun i _ -> i + t) ds)) :: r
	) [] rules in
	(fun i j -> (* Printf.printf "%s %d\n" i j; *) List.nth (List.assoc i v) j),!ls
*)


(* reduce_rules :: t番めのreduce規則についてで、 (ls,f) の組。 ls は縮約される記号の長さを、fはかかる関数を返す *)


let conv (header,precs,rules) =
	let _,type_token_s,token2id_s,tokens = conv_prec precs in
	let starts = (List.rev (collect_start precs)) in
	let precedences = collect_precedence precs in
	(* let rule2idx,rule_ls = get_rule2idx rules in *)
	let rule_ls = rules |> List.map (fun (_,d) -> d |> List.map (fun (x,_) -> List.length x)) in
	let sym2i,i2sym,i_sym_data = 
		let d = ref [] in 
		(fun x -> 
			match find_idx x !d with 
			| None -> (
					let r = List.length !d in 
					d := !d @ [x]; 
					Printf.printf "add %s as %d\n" x r;
					r
				) 
			| Some t -> t
		),
		(fun x -> List.nth !d x) ,
		d
	in
	let _ = List.map (fun (s,_) -> sym2i s) rules in
	let _ = tokens |> List.rev |> List.map (fun (x,_) -> sym2i x)  in (* これtokens上下逆になってんな *)
	let body = (
		header ^ 
		"type token = \n" ^ type_token_s ^ 
		"let token2id d = match d with\n" ^ token2id_s ^ "\n" ^
		(extract_folds_from_precs precs) ^
		(Printf.printf "extract_folds\n"; flush stdout; "") ^
		(extract_folds rules) ^ "\n" ^
		(Printf.printf "end make_tables\n"; flush stdout; "") ^
		(starts |> List.map (fun (s,_) -> 
				"let table_" ^ s ^ " = \n" ^ (make_table rules sym2i i2sym i_sym_data s precedences)
			) |> String.concat "") ^ "\n" ^
		(Printf.printf "start make_table\n"; flush stdout; "") ^
		"let rules = " ^ (list2str rule_ls (fun x -> list2str x string_of_int)) ^ "\n" ^
		(Printf.sprintf "let data2id d = match d with Token x -> (token2id x) + %d | Datum(i,_,_) -> i \n" (List.length rules)) ^
		(starts |> List.map (fun (s,_) -> 
				(* let my_parsing (istoplevel,rules,table) lexfun token2id =  *)
				Printf.sprintf 
					"let %s buf chan = fold_%s (Parsing.my_parsing ((fun i -> i = %d),rules,table_%s) (fun _ -> buf chan) data2id)\n" 
						s s (sym2i s) s
			) |> String.concat "") ^ "\n"
	) in
	Printf.printf "finish body\n";
	flush stdout;
	let head = (
		"type token = \n" ^ type_token_s ^
		(starts |> List.map (fun (s,t) -> 
				"val " ^ s ^ " : " ^ "(((unit -> char) * (int -> unit)) -> token) -> Lexing.lexbuf -> " ^  t
			) |> String.concat "") ^ "\n"
	) in
	(body,head)


let gen_int = let c = ref 0 in (fun () -> c := !c+1; !c)
