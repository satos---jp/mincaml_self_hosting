(*

Parser.toplevel Lexer.main (Lexing.from_channel stdin)


val my_parsing : data -> (unit -> 'a) -> ('a -> int) -> dataから決まる型
*)

(*
type 'a parsing_data = 
	| Token of 'a                         tokenそのままのやつ
	| Datum of int * int * data list      中間データiで規則jによって縮約された。
*)


type parsingact = 
	| Shift of int        (* 状態iに移動する *)
	| Reduce of int * int (* 中間データiの規則jで縮約する *)
	| Error

let rec split_list v p = 
	if p = 0 then ([],v) else 
	match v with
	| x :: xs -> (
			let (ra,rb) = split_list xs (p-1) in
			(x :: ra,rb)
		)

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

(*
let rec data2str token2id d = 
	let self = data2str token2id in
	match d with
	| Token(x) -> token2id x
	| Datum(i,xs) -> "(" ^ (token2id i) ^ "#" ^ (list2str xs self) ^ ")"
*)

let rec assoc_opt a v = 
	match v with
	| [] -> None
	| (b,x) :: xs -> if a = b then Some x else assoc_opt a xs

let rec data2str data2id d = 
	let self = data2str data2id in
	match d with
	| Token _ -> "Token[" ^ (string_of_int (data2id d)) ^ "]"
	| Datum(i,j,vs) -> "Datum(" ^ (string_of_int i) ^ "," ^ (string_of_int j) ^ "," ^  (list2str vs self) ^ ")"

let my_parsing (istoplevel,rulelens,table) lexfun data2id = 
	let autoact st = (* 状態stなら自動的にreduceする、みたいなやつ(入力の最後のため) *)
		match List.nth table st with
		| [d] -> (
				let (p,q) = d in 
				(*
					print_string ("actcheck " ^ (string_of_int p));
					print_char 10;
				*)
				if p = -1 then (match q with Reduce(i,j) -> Some((i,j))) 
				else None
			)
		| _ -> None
	in
	let get_act st x = (* 状態stからデータxが降ってきた際の挙動 *)
		let qt = List.nth table st in
		let act = assoc_opt (data2id x) qt in
		match act with
		| None -> raise_match_failure ("action with " ^ (string_of_int (data2id x)) ^ " from state " ^ (string_of_int st) ^ " failed")
		| Some x -> x
	in
	let rec steps vs st = 
		(*
		print_string ("state " ^ (string_of_int st));
		print_char 10;
		print_string (list2str vs (fun x -> string_of_int (data2id x)));
		print_char 10;
		*)
		match autoact st with
		| Some x -> (
				(*
				print_string ("autoact at state " ^ (string_of_int st));
				print_char 10;
				*)
				Some((x,true))
			)
		| None -> (
			match vs with
			| x :: xs -> (
					let act = get_act st x in
					match act with
					| Shift tst -> steps xs tst
					| Reduce(i,j) -> Some(((i,j),false))
				)
			| [] -> None
		)
	in
	let rec circle vs = 
		let st = 0 in
		let td = steps vs st in
		match td with
		| Some((i,j),isauto) -> (
				(*
				print_string ("reduce[" ^ (string_of_int i) ^ "," ^ (string_of_int j) ^ "]");
				*)
				let ls = List.nth (List.nth rulelens i) j in (* 後ろ1個除いた頭ls個を縮約する *)
				let als = List.length vs in
				(*
				print_string (" split_length " ^ (string_of_int ls));
				print_char 10;
				print_int ls;
				*)
				
				if isauto then (
					let (v,w) = split_list vs (als-ls) in
					let fv = Datum(i,j,w) in
					if istoplevel i then fv
					else circle (List.rev (v @ [fv]))
				) else (
					let (v,ws) = split_list vs (als-ls-1) in
					let (w,rw) = split_list ws ls in
					let fv = Datum(i,j,w) in
					if istoplevel i then fv
					else circle (v @ [fv] @ rw)
				)
			)
		| None -> circle (vs @ [Token (lexfun ())])
	in
	let res = circle [] in
	(*
	print_string (data2str data2id res);
	print_char 10;
	*)
	res



