open Virtual
open Syntax
open Type_checker
(* とりあえず、雑にx86コードを生成する *)

let constfs = ref ""

let gen_const = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@const_%d" !c)


let vs2stacks vs = 
	let rec f (ar,sl) vs = 
		match vs with
		| [] -> (ar,sl)
		| (na,ty) :: xs -> 
			let nl = 
				match ty with
				| TyFun(_,_) -> 8
				| _ -> 4
				(* 関数は(ポインタ,クロージャへのポインタ)で持ち、それ以外は1つで *)
			in
				((na,sl) :: ar,nl+sl)
	in
		f ([],0) vs

(*
[ebp-0x4] 以降 .. ローカル変数
[ebp] .. esp
[ebp+0x4] .. retaddr
[ebp+0x8] .. closure pointer
[ebp+0xc] 以下 .. 引数
で。
*)

let func2asm (fn,vs1,vs2,(ops,lvs)) = 
	let lvs_st = vs2stacks lvs in
	let vs1_st = vs2stacks vs1 in
	let vs2_st = vs2stacks vs2 in
	
	let espminus = (snd lvs_st) + 0x4 in
	let on_stack = 
		(List.map (fun (x,p) -> (x,-p-4)) (fst lvs_st)) @
		(List.map (fun (x,p) -> (x,p+(snd vs2_st)+8)) (fst vs2_st)) in
	let on_clos = fst vs1_st in
	let na2pt x = (
		try Printf.sprintf "[ebp%+d]" (List.assoc x on_stack) 
		with | Not_found -> 
		try Printf.sprintf "[ebp%+d]" (List.assoc x on_stack) 
		with | Not_found -> raise (Failure ("local value " ^ x ^ "doesn't find in neither vs1,vs2 nor lvs"))
	) in
 
	fn ^ ":\n" ^ 
	(String.concat "" (List.map (fun op -> 
		match op with
		| OpMovi((na,TyInt),CInt(v)) -> Printf.sprintf "mov %s,%d\n" na v
		| OpMovi((na,TyBool),CBool(v)) -> Printf.sprintf "mov %s,%d\n" na (if v then 1 else 0)
		| OpMovi((na,TyFloat),CFloat(v)) -> (
				let tag = gen_const () in
					constfs := (!constfs) ^ (Printf.sprintf "%s:\n	%f\n" tag v);
					Printf.sprintf "mov eax,%s\nmov %s eax" tag na
			)
		| OpMov((na,t1),(nb,t2)) -> assert (t1=t2); Printf.sprintf "mov eax,%s\nmov %s eax" na nb
		| OpLabel x -> x ^ ":\n"
	) ops))






