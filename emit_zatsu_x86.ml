open Virtual
open Syntax
open Type_checker
open Debug
(* とりあえず、雑にx86コードを生成する *)

let constfs = ref ""

let gen_const = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@const_%d" !c)


let vs2stacks vs = 
	let rec f (ar,sl) vs = 
		match vs with
		| [] -> (ar,sl)
		| (na,ty) :: xs -> 
			let nl = 4
				(* 関数は(クロージャへのポインタ,関数へのポインタ)で持ち、それ以外は1つで 
					 と思ったが、あまりにも面倒なので、ヒープに持ちます *)
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
	(*
	Printf.printf "%s%s%s" (names2str vs1) (names2str vs2) (names2str localvs);
	*)
	let lvs_st = vs2stacks localvs in
	let vs1_st = vs2stacks vs1 in
	let vs2_st = vs2stacks vs2 in
	
	let on_stack = 
		(List.map (fun (x,p) -> (x,p-(snd lvs_st))) (fst lvs_st)) @
		(List.map (fun (x,p) -> (x,p+8)) (fst vs2_st)) in
	let on_clos = fst vs1_st in
	print_string ("On function " ^ fn ^ "\n");
	print_string ((String.concat "\n" (List.map (fun (s,p) -> Printf.sprintf "%s :: [ebp%+d]" s p) on_stack)) ^"\n");
	print_string ((String.concat "\n" (List.map fst on_clos)) ^"\n");
	let na2pt x = (
		try ("ebp",List.assoc x on_stack)
		with | Not_found -> 
		try ("edi",List.assoc x on_clos)
		with | Not_found -> ("@" ^ x,-1)
	) in
	let pt2s (a,b) = 
		if String.get a 0 = '@' then String.sub a 1 ((String.length a)-1) else 
			Printf.sprintf "dword [%s%+d]" a b 
	in
	let na2s x = pt2s (na2pt x) in
	let make_vs_on_heap vs = (
		let nl = ref 0 in
		let cs = 
			(String.concat "" (List.map (fun (na,nt) ->
			let (p,l) = na2pt na in
				nl := !nl + 4;
				(Printf.sprintf "\tmov eax,%s\n\tmov dword [esi%+d],eax\n" (pt2s (p,l)) (!nl-4))
			) vs)) in
		cs ^ (Printf.sprintf "\tadd esi,%d\n" !nl)
	) in
	let prologue = (Printf.sprintf "\tpush ebp\n\tmov ebp,esp\n\tsub esp,%d\n" (snd lvs_st)) ^	
		if fn = "_main" then "\tmov esi,global_heap\n" else ""
	in
	let epilogue = (Printf.sprintf "\tadd esp,%d\n\tpop ebp\n\tret\n" (snd lvs_st)) in
	
	let mova2b (na,ta) (nb,tb) = 
		let (p1,l1) = na2pt na in
		let (p2,l2) = na2pt nb in
			(Printf.sprintf "\tmov eax,%s\n" (pt2s (p2,l2))) ^
			(Printf.sprintf "\tmov %s,eax\n" (pt2s (p1,l1)))
	in
	
	let unopr2s nr na s = 
		(Printf.sprintf "\tmov eax,%s\n" (na2s na)) ^
		s ^ 
		(Printf.sprintf "\tmov %s,eax\n" (na2s nr))
	in
	let biopr2s nr na nb s = 
		(Printf.sprintf "\tmov eax,%s\n" (na2s na)) ^
		(Printf.sprintf "\tmov ebx,%s\n" (na2s nb)) ^
		s ^ 
		(Printf.sprintf "\tmov %s,eax\n" (na2s nr))
	in
	let triopr2s nr na nb nc s = 
		(Printf.sprintf "\tmov eax,%s\n" (na2s na)) ^
		(Printf.sprintf "\tmov ebx,%s\n" (na2s nb)) ^
		(Printf.sprintf "\tmov ecx,%s\n" (na2s nc)) ^
		s ^ 
		(Printf.sprintf "\tmov %s,eax\n" (na2s nr))
	in
	let fbiopr2s nr na nb s = 
		(Printf.sprintf "\tfld %s\n" (na2s na)) ^
		(Printf.sprintf "\tfld %s\n" (na2s nb)) ^
		s ^ 
		(Printf.sprintf "\tfstp %s\n" (na2s nr))
	in
	
	fn ^ ":\n" ^ prologue ^ 
	(String.concat "" (List.map (fun op -> 
		match op with
		| OpMovi((na,(t,d)),CInt(v)) -> assert (t=TyInt); 
			(Printf.sprintf "\tmov %s,%d\n" (na2s na) v) ^ "; " ^ (debug_data2simple d) ^ "\n"
		| OpMovi((na,(t,d)),CBool(v)) -> assert (t=TyBool); 
			(Printf.sprintf "\tmov %s,%d\n" (na2s na) (if v then 1 else 0)) ^ "; " ^ (debug_data2simple d) ^ "\n"
		| OpMovi((na,(t,d)),CFloat(v)) -> assert (t=TyFloat); (
				let tag = gen_const () in
					constfs := (!constfs) ^ (Printf.sprintf "%s:\n\tdd %f\n" tag v);
					Printf.sprintf "\tmov eax,[%s]\n\tmov %s,eax\n" tag (na2s na)
			)
		| OpMov((na,(t1,d1)),(nb,(t2,d2))) -> assert (t1=t2); 
			(mova2b (na,t1) (nb,t2)) ^ "; " ^ (debug_data2simple d1) ^ (debug_data2simple d2) ^ "\n"
		| OpLabel x -> x ^ ":\n"
		| OpJcnd(ct,(na,_),(nb,_),la) -> (
				(Printf.sprintf "\tmov eax,%s\n\tmov ebx,%s\n" (na2s na) (na2s nb)) ^ 
				(Printf.sprintf "\tcmp eax,ebx\n\t%s %s\n" (match ct with CmpEq -> "jne" | CmpLt -> "jl") la)
			)
		| OpJmp(la) -> Printf.sprintf "\tjmp %s\n" la
		| OpDestTuple(vs,(tna,t)) -> (
				let nl = ref 0 in
				(Printf.sprintf "\tmov eax,%s\n" (na2s tna)) ^ 
				(String.concat "" (List.map (fun (na,nt) -> 
					let (p,l) = na2pt na in
						nl := !nl + 4;
						(Printf.sprintf "\tmov ebx,dword [eax%+d]\n\tmov %s,ebx\n" (!nl-4) (pt2s (p,l)))
					) vs))
			)
		| OpMakeTuple((na,t),vs) -> (
				(Printf.sprintf "\tmov %s,esi\n" (na2s na)) ^ 
				(make_vs_on_heap vs)
			)
		| OpMakeCls((na,(_,d1)),(fn,(_,d2)),vs) -> (
				"\tmov edx,esi\n" ^ 
				(make_vs_on_heap vs) ^
				"\tmov dword [esi+4],edx\n" ^ 
				(Printf.sprintf "\tmov dword [esi],%s\n" fn) ^ 
				(Printf.sprintf "\tmov %s,esi\n" (na2s na)) ^ 
				"\tadd esi,8\n"^
				"; " ^ (debug_data2simple d1) ^ " "^ (debug_data2simple d2) ^ "\n"
			)
		| OpSelfCls((na,t),(fn,ft)) -> (
				(Printf.sprintf "\tmov dword [esi],%s\n" fn) ^ 
				"\tmov dword [esi+4],-1\n" ^ 
				(Printf.sprintf "\tmov %s,esi\n" (na2s na)) ^ 
				"\tadd esi,8\n"
			)
		| OpApp((na,nt),(fn,ft),vs) -> (
				let nl = ref 0 in
				let s = 
				"\tpush edi\n" ^
				(String.concat "" (List.map (fun (na,nt) -> 
					let (p,l) = na2pt na in
						nl := !nl + 4;
						(Printf.sprintf "\tpush dword %s\n" (pt2s (p,l)))
					) (List.rev vs))) ^ (* 逆にpushする *)
				(let rfn = (na2s fn) in
					if List.mem fn global_funcs then 
					(Printf.sprintf "\tcall %s\n" fn)
				else 
					((Printf.sprintf "\tmov eax,%s\n" rfn) ^ 
					"\tmov edi,[eax+4]\n" ^ 
					"\tcall [eax]\n")) ^
				(let (p,l) = na2pt na in
						(Printf.sprintf "\tmov %s,eax\n" (pt2s (p,l)))
					) in
				s ^ 
				(Printf.sprintf "\tadd esp,%d\n" !nl) ^
				"\tpop edi\n"
			)
		| OpRet((na,nt)) -> (
				(let (p,l) = na2pt na in
					(Printf.sprintf "\tmov eax,%s\n" (pt2s (p,l)))
					) ^
				epilogue
			)
		| OpMainRet -> (
				"\tmov eax,0\n" ^
				epilogue
			)
		| OpOpr((nr,_),Ominus,[(na,_)]) -> (
				unopr2s nr na "\tneg eax\n"
			)
		| OpOpr((nr,_),Onot,[(na,_)]) -> (
				unopr2s nr na "\ttest eax,eax\n\tsete al\n\tand eax,1\n"
			)
		| OpOpr((nr,_),Oadd,[(na,_);(nb,_)]) -> (
				biopr2s nr na nb "\tadd eax,ebx\n"
			)
		| OpOpr((nr,_),Omul,[(na,_);(nb,_)]) -> (
				biopr2s nr na nb "\tmul ebx\n"
			)
		| OpOpr((nr,_),Osub,[(na,_);(nb,_)]) -> (
				biopr2s nr na nb "\tsub eax,ebx\n"
			)
		| OpOpr((nr,_),Odiv,[(na,_);(nb,_)]) -> (
				biopr2s nr na nb "\txor edx,edx\n\tdiv ebx\n"
			)
		| OpOpr((nr,_),Olt,[(na,_);(nb,_)]) -> (
				biopr2s nr na nb "\txor ecx,ecx\n\tcmp eax,ebx\n\tsetl cl\n\tmov eax,ecx\n"
			)
		| OpOpr((nr,_),Oleq,[(na,_);(nb,_)]) -> (
				biopr2s nr na nb "\txor ecx,ecx\n\tcmp eax,ebx\n\tsetle cl\n\tmov eax,ecx\n"
			)
		| OpOpr((nr,_),Ogt,[(na,_);(nb,_)]) -> (
				biopr2s nr na nb "\txor ecx,ecx\n\tcmp eax,ebx\n\tsetg cl\n\tmov eax,ecx\n"
			)
		| OpOpr((nr,_),Ogeq,[(na,_);(nb,_)]) -> (
				biopr2s nr na nb "\txor ecx,ecx\n\tcmp eax,ebx\n\tsetge cl\n\tmov eax,ecx\n"
			)
		| OpOpr((nr,_),Oeq,[(na,_);(nb,_)]) -> (
				biopr2s nr na nb "\txor ecx,ecx\n\tcmp eax,ebx\n\tsete cl\n\tmov eax,ecx\n"
			)
		| OpOpr((nr,_),Oneq,[(na,_);(nb,_)]) -> (
				biopr2s nr na nb "\txor ecx,ecx\n\tcmp eax,ebx\n\tsetne cl\n\tmov eax,ecx\n"
			)
		| OpOpr((nr,_),Ofadd,[(na,_);(nb,_)]) -> (
				fbiopr2s nr na nb "\tfaddp\n"
			)
		| OpOpr((nr,_),Ofmul,[(na,_);(nb,_)]) -> (
				fbiopr2s nr na nb "\tfmulp\n"
			)
		| OpOpr((nr,_),Ofsub,[(na,_);(nb,_)]) -> (
				fbiopr2s nr na nb "\tfsubp\n"
			)
		| OpOpr((nr,_),Ofdiv,[(na,_);(nb,_)]) -> (
				fbiopr2s nr na nb "\tfdivp\n"
			)
		| OpOpr(nr,Osemi2,[_;nb]) -> mova2b nr nb
		| OpOpr(nr,Osemi1,[nb]) -> mova2b nr nb
		| OpOpr((nr,_),OArrCrt,[(na,_);(nb,_)]) -> (
				biopr2s nr na nb (
					let la = genlabel () in
					let lb = genlabel () in
					"\tmov edx,esi\n" ^
					"\ttest eax,eax\n" ^
					(Printf.sprintf "\tje %s\n" lb) ^
					(Printf.sprintf "%s:\n" la) ^ 
					"\tmov dword [esi],ebx\n" ^
					"\tadd esi,4\n" ^
					"\tsub eax,1\n" ^
					(Printf.sprintf "\tjne %s\n" la) ^
					(Printf.sprintf "%s:\n" lb) ^ 
					"\tmov eax,edx\n"
				)
			)
		| OpOpr((nr,_),OArrRead,[(na,_);(nb,_)]) -> (
				biopr2s nr na nb "\tmov eax,dword [eax+4*ebx]\n"
			)
		| OpOpr((nr,_),OArrWrite,[(na,_);(nb,_);(nc,_)]) -> (
				triopr2s nr na nb nc "\tmov dword [eax+4*ebx],ecx\n"
			)
		| OpOpr(_,x,vs) -> raise (Failure (Printf.sprintf "Operation %s with %d argument in not defined yet" (op2str x) (List.length vs)))
	) ops))

(*
 nasm out.s -f elf32 -g -o out.o; gcc -m32 out.o
 nasm out.s -f win32 -g -o out.o; gcc -m32 out.o

*)

let vir2asm (funs,rd) = 
	"BITS 32\n" ^
	"%include \"libio_win.s\"\n" ^
	"%include \"lib.s\"\n" ^
	"section .data\n" ^
	!constfs ^
	"section .bss\n" ^
	"global_heap:\n" ^
	"\tresb 0x10000000\n" ^
	"section .text\n" ^
	"global _main\n" ^ 
	(String.concat "" (List.map func2asm (List.rev funs))) ^	
	(func2asm (("_main",TyVar(-1)),[],[],rd))




