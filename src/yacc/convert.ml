open Syntax

(* PType... -> mliを作るためにいるっぽい(parserはmliを作れる!!) *)

let collect_start pcs = 
	List.fold_left (fun v pc -> 
		match pc with
		| PStart x -> x :: v
		| _ -> v
	) [] pcs

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


type parsingact = 
	| Shift of int * int
	| Reduce of int * int

type rl_item = (int * int) * (int list) * (int list) (* symbol/rule/dotより前/dotより後 *)


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
		| [] -> failwith "assoc_idx failure"
		| (x,d) :: xs -> if x = a then (i,d) else f xs (i+1)
	in f v 0
	
let enumerate v = List.mapi (fun i x -> (x,i)) v






let resum2str v = 
	match v with
	| Reduce(i,j) -> Printf.sprintf "Reduce[%d,%d]" i j
	| Shift(i,j) -> Printf.sprintf "Shift[%d,%d]" i j

(* table :: 状態stでi番めのデータが降ってきた際にshift/reduceするやつ *)
let make_table rules sym2i i2sym tokens startsym = 
	let rl2str ((i,j),ss,ts) = (
	(*
		(Printf.sprintf "{(%d,%d),%s,%s} " i j (list2str ss string_of_int) (list2str ts string_of_int)) ^
	*)
		(Printf.sprintf "{(%s,%d),%s,%s}" (i2sym i) j (list2str ss i2sym) (list2str ts i2sym))
	)
	in
	let irules = List.map (fun (s,d) -> (* Printf.printf "rule %s idx %d\n" s (sym2i s); *) sym2i s,d) rules in
	let rec saturate v ds = 
		(* Printf.printf "%s\n" (rl2str d); *)
		let self = saturate in
		match ds with
		| [] -> v
		| d :: rds -> (
				let v = self v rds in
				if List.mem d v then v
				else match d with
				| _,_,[] -> d :: v
				| _,_,x :: xs -> (
					(* Printf.printf "search by %d\n" x; *)
					let nru = try List.assoc x irules with Not_found -> [] in
					List.fold_left (fun r ((syms,_),j) -> 
						let isyms = List.map sym2i syms in
						self r [((x,j),[],isyms)]
					) (d :: v) (enumerate nru)
				)
			)
	in
	let vs = ref [] in
	let es = ref [] in
	let rec add_state_node it = 
		let self = add_state_node in
		let tit = saturate [] it in
		match find_idx tit (!vs) with
		| Some t -> t
		| None -> (
				let rt = List.length !vs in
				vs := (!vs) @ [tit];
				let ne = ref [] in
				es := !es @ [ne];
				ne := List.concat (List.map (fun ((p,q),be,af) -> 
					match af with
					| [] -> [Reduce(p,q)]
					| _ -> []
				) tit);
				
				let samexs = ref [] in
				List.iter (fun ((p,q),be,af) -> 
					match af with
					| [] -> ()
					| x :: xs -> (
							let d = ((p,q),be @ [x],xs) in
							try 
								let v = List.assoc x !samexs in
								v := d :: !v
							with
								| Not_found -> samexs := (x,ref [d]) :: !samexs
						)
				) tit;
				ne := !ne @ (List.map (fun (x,ds) -> let nt = self !ds in Shift(x,nt)) !samexs);
				rt
			)
	in
	let stp = add_state_node [(((sym2i "@start_start"),-1),[],[sym2i startsym])] in
	
	(* let v = x :: xs in List.nth (List.length xs) が x と違うの罠過ぎませんか??? *)
	
	let ds = 
		(List.combine !vs !es) |> 
		(List.mapi (fun i (v,e) ->
			(Printf.sprintf "state %d:\n\t" i) ^ 
			(v |>
			(List.map rl2str) |>
			String.concat "\n\t") ^ 
			"\nwith edge\n" ^
			(list2str (!e) (fun d -> 
				match d with
				| Reduce(i,j) -> Printf.sprintf "Reduce[%s,%d]" (i2sym i) j
				| Shift(i,j) -> Printf.sprintf "Shift[%s,%d]" (i2sym i) j
			)) ^ "\n"
		)) |>
		(String.concat "")
  in
	Printf.printf "%s\n" ds;
	(*
	list2str !es (fun e -> 
		list2str e (fun d -> 
			resum2str d
		)
	) *)
	""




let gen_int = let c = ref 0 in (fun () -> c := !c+1; !c)


(* reduce_rules :: t番めのreduce規則についてで、 (ls,f) の組。 ls は縮約される記号の長さを、fはかかる関数を返す *)


let conv (header,precs,rules) =
	let _,type_token_s,token2id_s = conv_prec precs in
	let starts = collect_start precs in
	let sym2i,i2sym = 
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
		(fun x -> List.nth !d x) 
	in
	header ^ 
	"type token = \n" ^ type_token_s ^ 
	"let token2id d = match d with\n" ^ token2id_s ^ "\n" ^
	(extract_folds_from_precs precs) ^
	(extract_folds rules) ^
	(starts |> List.map (fun s -> 
			"let table_" ^ s ^ " = \n" ^ (make_table rules sym2i i2sym () s)
		) |> String.concat "")




