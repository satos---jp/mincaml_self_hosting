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
				(* 関数は(クロージャへのポインタ,関数へのポインタ)で持ち、それ以外は1つで *)
			in
				f ((na,sl) :: ar,nl+sl) xs
	in
		f ([],0) vs

(*
[ebp-0x4] 以降 .. ローカル変数
[ebp] .. esp
[ebp+0x4] .. retaddr
[ebp+0x8] 以下 .. 引数
で。

とりあえず、ediにクロージャポインタは持っておいて、
call時にpushしたりする
あと、esiにヒープへのポインタでも持っておきますか


返り値は、ふつう、eaxに。関数の場合は、eax,ebxで。
*)

let func2asm ((fn,_),vs1,vs2,(ops,localvs)) = 
	Printf.printf "%s%s%s" (names2str vs1) (names2str vs2) (names2str localvs);
	let lvs_st = vs2stacks localvs in
	let vs1_st = vs2stacks vs1 in
	let vs2_st = vs2stacks vs2 in
	
	let on_stack = 
		(List.map (fun (x,p) -> (x,-p-4)) (fst lvs_st)) @
		(List.map (fun (x,p) -> (x,p+(snd vs2_st)+8)) (fst vs2_st)) in
	let on_clos = fst vs1_st in
	print_string ((String.concat "," (List.map fst on_stack)) ^"\n");
	print_string ((String.concat "," (List.map fst on_clos)) ^"\n");
	let na2pt x = (
		try ("ebp",List.assoc x on_stack)
		with | Not_found -> 
		try ("edi",List.assoc x on_clos)
		with | Not_found -> raise (Failure ("local value " ^ x ^ " doesn't find in neither vs1,vs2 nor lvs"))
	) in
	let pt2s (a,b) = Printf.sprintf "dword [%s%+d]" a b in
	let na2s x = pt2s (na2pt x) in
	
	let make_vs_on_heap vs = (
		let nl = ref 0 in
		let cs = 
			(String.concat "" (List.map (fun (na,nt) -> 
			let (p,l) = na2pt na in
			match nt with
			| TyFun(_,_) -> (
					nl := !nl + 8;
					(Printf.sprintf "\tmov ebx,%s\n\tmov dword [esi%+d],ebx\n" (pt2s (p,l)) (!nl-8) ) ^
					(Printf.sprintf "\tmov ebx,%s\n\tmov dword [esi%+d],ebx\n" (pt2s (p,l+4)) (!nl-4))
				)
			| _ -> (
					nl := !nl + 4;
					(Printf.sprintf "\tmov ebx,%s\n\tmov dword [esi%+d],ebx\n" (pt2s (p,l)) (!nl-4))
				)
			) vs)) in
		(Printf.sprintf "\tadd esi,%d\n" !nl) ^ cs
	) in
	let prologue = (Printf.sprintf "\tpush ebp\n\tmov ebp,esp\n\tsub esp,%d\n" (snd vs2_st)) in
	let epilogue = (Printf.sprintf "\tadd esp,%d\n\tpop ebp\n\tret\n" (snd vs2_st)) in
	
	fn ^ ":\n" ^ prologue ^ 
	(String.concat "" (List.map (fun op -> 
		match op with
		| OpMovi((na,TyInt),CInt(v)) -> Printf.sprintf "\tmov %s,%d\n" (na2s na) v
		| OpMovi((na,TyBool),CBool(v)) -> Printf.sprintf "\tmov %s,%d\n" (na2s na) (if v then 1 else 0)
		| OpMovi((na,TyFloat),CFloat(v)) -> (
				let tag = gen_const () in
					constfs := (!constfs) ^ (Printf.sprintf "%s:\n\t%f\n" tag v);
					Printf.sprintf "\tmov eax,[%s]\n\tmov %s eax\n" tag (na2s na)
			)
		| OpMov((na,t1),(nb,t2)) -> assert (t1=t2); Printf.sprintf "\tmov eax,%s\n\tmov %s eax\n" na nb
		| OpLabel x -> x ^ ":\n"
		| OpJcnd(ct,(na,_),(nb,_),la) -> (
				(Printf.sprintf "\tmov eax,%s\n\tmov ebx,%s\n" (na2s na) (na2s nb)) ^ 
				(Printf.sprintf "\tcmp eax,ebx\n\t%s %s\n" (match ct with CmpEq -> "jeq" | CmpLt -> "jl") la)
			)
		| OpJmp(la) -> Printf.sprintf "\tjmp %s\n" la
		| OpDestTuple(vs,(tna,t)) -> (
				let nl = ref 0 in
				(Printf.sprintf "\tmov eax,%s\n" (na2s tna)) ^ 
				(String.concat "" (List.map (fun (na,nt) -> 
					let (p,l) = na2pt na in
					match nt with
					| TyFun(_,_) -> (
							nl := !nl + 8;
							(Printf.sprintf "\tmov ebx,dword [eax%+d]\n\tmov %s ebx\n" (!nl-8) (pt2s (p,l))) ^
							(Printf.sprintf "\tmov ebx,dword [eax%+d]\n\tmov %s ebx\n" (!nl-4) (pt2s (p,l+4)))
						)
					| _ -> (
							nl := !nl + 4;
							(Printf.sprintf "\tmov ebx,dword [eax%+d]\n\tmov %s ebx\n" (!nl-4) (pt2s (p,l)))
						)
					) vs))
			)
		| OpMakeTuple((na,t),vs) -> (
				(Printf.sprintf "\tmov %s,esi\n" (na2s na)) ^ 
				(make_vs_on_heap vs)
			)
		| OpMakeCls((na,t),(fn,ft),vs) -> (
				let (p,l) = na2pt na in
				(Printf.sprintf "\tmov %s,esi\n" (na2s na)) ^ 
				(make_vs_on_heap vs) ^
				(Printf.sprintf "\tmov %s,%s\n" (pt2s (p,l+4)) fn)
			)
		| OpApp((na,nt),(fn,ft),vs) -> (
				let nl = ref 0 in
				"\tpush edi\n" ^
				(String.concat "" (List.map (fun (na,nt) -> 
					let (p,l) = na2pt na in
					match nt with
					| TyFun(_,_) -> (
							(Printf.sprintf "\tpush dword %s\n" (pt2s (p,l))) ^
							(Printf.sprintf "\tpush dword %s\n" (pt2s (p,l+4)))
						)
					| _ -> (
							nl := !nl + 4;
							(Printf.sprintf "\tpush dword %s\n" (pt2s (p,l)))
						)
					) vs)) ^
				(let (p,l) = na2pt na in
					(Printf.sprintf "\tmov edi,%s\n" (pt2s (p,l))) ^ 
					(Printf.sprintf "\tcall %s\n" (pt2s (p,l+4)))) ^
				(let (p,l) = na2pt na in
					match nt with
					| TyFun(_,_) -> (
							(Printf.sprintf "\tmov %s,eax\n" (pt2s (p,l))) ^
							(Printf.sprintf "\tmov %s,ebx\n" (pt2s (p,l+4)))
						)
					| _ -> (
							(Printf.sprintf "\tmov %s,eax\n" (pt2s (p,l)))
						)) ^
				(Printf.sprintf "\tsub esp,%d\n" !nl) ^
				"\tpop edi\n"
			)
		| OpRet((na,nt)) -> (
				(let (p,l) = na2pt na in
					match nt with
					| TyFun(_,_) -> (
							(Printf.sprintf "\tmov eax,%s\n" (pt2s (p,l))) ^
							(Printf.sprintf "\tmov ebx,%s\n" (pt2s (p,l+4)))
						)
					| _ -> (
							(Printf.sprintf "\tmov eax,%s\n" (pt2s (p,l)))
						)) ^
				epilogue
			)
		| OpMainRet -> (
				"\tmov eax,0\n" ^
				epilogue
			)
		| OpOpr((nr,_),Oadd,[(na,_);(nb,_)]) -> (
				(Printf.sprintf "\tmov eax,%s\n" (na2s na)) ^
				(Printf.sprintf "\tmov ebx,%s\n" (na2s nb)) ^
				"\tadd eax,ebx\n" ^ 
				(Printf.sprintf "\tmov %s,eax\n" (na2s nr))
			)
	) ops))



let vir2asm (funs,rd) = 
	"BITS 32\n" ^
	!constfs ^
	"section .text\n" ^
	"global main\n" ^ 
	(String.concat "" (List.map func2asm funs)) ^	
	(func2asm (("main",TyVar(-1)),[],[],rd))




