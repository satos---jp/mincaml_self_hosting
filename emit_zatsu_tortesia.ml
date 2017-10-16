open Virtual
open Syntax
open Type_checker
open Debug
(* とりあえず、雑に tortesia コードを生成する *)

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

全体のglobalな値への参照をどないしよう
全体を let rec g () = ... in g () とすればよさそう？(雑)


とりあえず、

r0 :: esp
r1 :: ebp
r2 :: esi (ヒープポインタ)
r3 :: edi (クロージャへのポインタ)

とします。
*)


let main_name = "main"

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
	print_string ((String.concat "\n" (List.map (fun (s,p) -> Printf.sprintf "%s :: [r1%+d]" s p) on_stack)) ^"\n");
	print_string ((String.concat "\n" (List.map (fun (s,p) -> Printf.sprintf "%s :: [r3%+d]" s p) on_clos)) ^"\n");
	let na2pt x = (
		try ("r1",List.assoc x on_stack)
		with | Not_found -> 
		try ("r3",List.assoc x on_clos)
		with | Not_found -> ("@" ^ x,-1)
	) in
	let pt2s (a,b) = 
		if String.get a 0 = '@' then String.sub a 1 ((String.length a)-1) else 
			Printf.sprintf "[%s%+d]" a b 
	in
	let na2s x = pt2s (na2pt x) in
	let nd2ps (na,_) = na2s na in
	let nd2ds (_,(_,d)) = (debug_data2simple d) in
	let make_vs_on_heap vs = (
		let nl = ref 0 in
		let cs = 
			(String.concat "" (List.map (fun (na,nt) ->
			let (p,l) = na2pt na in
				nl := !nl + 4;
				(Printf.sprintf "\tlw r4,%s\n\tsw [r2%+d],r4\n" (pt2s (p,l)) (!nl-4))
			) vs)) in
		cs ^ (Printf.sprintf "\taddi r4,%d\n" !nl)
	) in
	let prologue = 
		"\tsubi r0,4\n" ^ 
		"\tsw [r0],lr\n" ^ 
		(Printf.sprintf "\tsubi r0,4\n\tsw [r0],r1\n\tmov r1,r0\n\tsubi r0,%d\n" (snd lvs_st)) ^	
		(if fn = main_name then "\tmov r2,global_heap\n" else "")
	in
	let epilogue = (
		(Printf.sprintf "\taddi r0,%d\n\taddi r0,4\n\tlw r1,[r0]\n" (snd lvs_st)) ^
		"\taddi r0,4\n" ^ 
		"\tlw r5,[r0]\n" ^
		"\tjr r5\n"
	)
	in
	let mova2b nad nbd = 
			(Printf.sprintf "\tlw r4,%s\n" (nd2ps nbd)) ^
			(Printf.sprintf "\tsw %s,r4\n" (nd2ps nad)) ^
			"; " ^ (nd2ds nad) ^ " ::<= " ^ (nd2ds nbd) ^ "\n"
	in
	let unopr2s nr na s = 
		(Printf.sprintf "\tlw r4,%s\n" (nd2ps na)) ^
		s ^ 
		(Printf.sprintf "\tsw %s,r4\n" (nd2ps nr))
	in
	let biopr2s nr na nb s = 
		(Printf.sprintf "\tlw r5,%s\n" (nd2ps nb)) ^
		(unopr2s nr na s)
	in
	let fbiopr2s nr na nb s = 
		raise (Failure "fbiopr2s is not impremented yet")
(*
		(Printf.sprintf "\tfld %s\n" (nd2ps na)) ^
		(Printf.sprintf "\tfld %s\n" (nd2ps nb)) ^
		s ^ 
		(Printf.sprintf "\tfstp %s\n" (nd2ps nr))
*)
	in
	let triopr2s nr na nb nc s = 
		(Printf.sprintf "\tlw r6,%s\n" (nd2ps nc)) ^
		(biopr2s nr na nb s)
	in
	
	fn ^ ":\n" ^ prologue ^ 
	(String.concat "" (List.map (fun op -> 
		match op with
		| OpMovi((na,(t,d)),CInt(v)) -> assert (t=TyInt); 
			(Printf.sprintf "\tli r4,%d\n" v) ^ 
			(Printf.sprintf "\tsw %s,r4\n" (na2s na)) ^ 
			"; " ^ (debug_data2simple d) ^ "\n"
		| OpMovi((na,(t,d)),CBool(v)) -> assert (t=TyBool); 
			(Printf.sprintf "\tli r4,%d\n" (if v then 1 else 0)) ^
			(Printf.sprintf "\tsw %s,r4\n" (na2s na)) ^
			"; " ^ (debug_data2simple d) ^ "\n"
		| OpMovi(_,CFloat(v)) -> raise (Failure "Movi Cfloat is not implemented yet")
(*
		| OpMovi((na,(t,d)),CFloat(v)) -> assert (t=TyFloat); (
				let tag = gen_const () in
					constfs := (!constfs) ^ (Printf.sprintf "%s:\n\tdd %f\n" tag v);
					Printf.sprintf "\tmov eax,[%s]\n\tmov %s,eax\n" tag (na2s na)
			)
*)
		| OpMov(((na,(t1,d1)) as nrd),((nb,(t2,d2)) as nad)) -> assert (t1=t2); (mova2b nrd nad) 
		| OpLabel x -> x ^ ":\n"
		| OpJcnd(ct,(na,_),(nb,_),la) -> (
				(Printf.sprintf "\tlw r4,%s\n\tlw r5,%s\n" (na2s na) (na2s nb)) ^ 
				(Printf.sprintf "\t%s r4,r5,%s\n" (match ct with CmpEq -> "jne" | CmpLt -> "jl") la)
			)
		| OpJmp(la) -> Printf.sprintf "\tjmp %s\n" la
		| OpDestTuple(vs,nad) -> (
				let nl = ref 0 in
				(Printf.sprintf "\tlw r4,%s\n" (nd2ps nad)) ^ 
				(String.concat "" (List.map (fun (na,nt) -> 
					let (p,l) = na2pt na in
						nl := !nl + 4;
						(Printf.sprintf "\tlw r5,[r4%+d]\n\tsw %s,r5\n" (!nl-4) (pt2s (p,l)))
					) vs)) ^
				"; " ^ (nd2ds nad) ^ "\n"
			)
		| OpMakeTuple(nad,vs) -> (
				(Printf.sprintf "\tsw %s,r2\n" (nd2ps nad)) ^ 
				(make_vs_on_heap vs) ^ 
				"; " ^ (nd2ds nad) ^ "\n"
			)
		| OpMakeCls(nad,((fn,_) as fnd),vs) -> (
				"\tmov r5,r2\n" ^ 
				(make_vs_on_heap vs) ^
				"\tsw [r2+4],r5\n" ^ 
				(Printf.sprintf "\tli r4,%s\n" fn) ^ 
				"\tsw [r2],r4\n" ^ 
				(Printf.sprintf "\tsw %s,r2\n" (nd2ps nad)) ^ 
				"\taddi r2,8\n"^
				"; " ^ (nd2ds nad) ^ " "^ (nd2ds fnd) ^ "\n"
			)
		| OpSelfCls(nad,((fn,_) as fnd)) -> (
				(Printf.sprintf "\tli r4,%s\n" fn) ^ 
				"\tsw [r2],r4\n" ^
				"\tli r4,-1\n" ^
				"\tsw [r2+4],r4\n" ^ 
				(Printf.sprintf "\tsw %s,r2\n" (nd2ps nad)) ^ 
				"\taddi r2,8\n" ^
				"; " ^ (nd2ds nad) ^ " " ^ (nd2ds fnd) ^ "\n"
			)
		| OpApp(nad,((fn,_) as fnd),vs) -> (
				let nl = ref 0 in
				let s = 
				"\tsubi r0,4\n" ^
				"\tsw [r0],r3\n" ^
				(String.concat "" (List.map (fun nad -> 
					nl := !nl + 4;
					"\tsubi r0,4\n" ^
					(Printf.sprintf "\tsw [r0],%s\n" (nd2ps nad))
				) (List.rev vs))) ^ (* 逆にpushする *)
				(let rfn = (na2s fn) in
					if List.mem fn global_funcs then 
					(Printf.sprintf "\tj %s\n" fn)
				else 
					((Printf.sprintf "\tlw r4,%s\n" rfn) ^ 
					"\tlw r3,[r4+4]\n" ^ 
					"\tlw r4,[r4]\n" ^ 
					"\tjr r4\n")) ^
				(Printf.sprintf "\tsw %s,r4\n" (nd2ps nad))
				in (* こうしないと、nlがアップデートされない *)
				s ^ 
				(Printf.sprintf "\taddi r0,%d\n" !nl) ^
				"\tlw r3,[r0]\n" ^
				"\tsubi r0,4\n" ^
				"; " ^ (nd2ds nad) ^ " " ^ (nd2ds fnd) ^ "\n"
			)
		| OpRet((na,nt)) -> (
				(let (p,l) = na2pt na in
					(Printf.sprintf "\tlw r4,%s\n" (pt2s (p,l)))
					) ^
				epilogue
			)
		| OpMainRet -> (
				"\tli r4,0\n" ^
				epilogue
			)
		| OpOpr(nrd,Osemi2,[_;nbd]) -> mova2b nrd nbd
		| OpOpr(nrd,Osemi1,[nad]) -> mova2b nrd nad
(*
		| OpOpr(nrd,op,[nad]) -> (
				let os = op2str op in
				(unopr2s nrd nad (
					match op with
					| Ominus -> "\tneg eax\n"
					| Onot   -> "\ttest eax,eax\n\tsete al\n\tand eax,1\n"
					| _ -> raise (Failure (Printf.sprintf "Operation %s is not unary operation" os))
			 	)) ^
				"; " ^ (nd2ds nrd) ^ " ::= " ^ os ^ " " ^ (nd2ds nad) ^ "\n"
			)
*)
		| OpOpr(nrd,op,[nad;nbd]) -> (
				let os = op2str op in
				(if List.mem op [Ofadd;Ofsub;Ofmul;Ofdiv] then
					fbiopr2s nrd nad nbd (
						match op with
					(*
						| Ofadd -> "\tfaddp\n"
						| Ofsub -> "\tfsubp\n"
						| Ofmul -> "\tfmulp\n"
						| Ofdiv -> "\tfdivp\n"
					*)
						| _ -> raise (Failure (Printf.sprintf "Operation %s is not float binary operation" os))
					) 
				else
					biopr2s nrd nad nbd (
						match op with
						| Oadd -> "\tadd r4,r4,r5\n"
						| Osub -> "\tsub r4,r4,r5\n"
					(*
						| Omul -> "\tmul ebx\n"
						| Odiv -> "\txor edx,edx\n\tdiv ebx\n"
						| Olt  -> "\txor ecx,ecx\n\tcmp eax,ebx\n\tsetl cl\n\tmov eax,ecx\n"
						| Oleq -> "\txor ecx,ecx\n\tcmp eax,ebx\n\tsetle cl\n\tmov eax,ecx\n"
						| Ogt  -> "\txor ecx,ecx\n\tcmp eax,ebx\n\tsetg cl\n\tmov eax,ecx\n"
						| Ogeq -> "\txor ecx,ecx\n\tcmp eax,ebx\n\tsetge cl\n\tmov eax,ecx\n"
						| Oeq  -> "\txor ecx,ecx\n\tcmp eax,ebx\n\tsete cl\n\tmov eax,ecx\n"
						| Oneq -> "\txor ecx,ecx\n\tcmp eax,ebx\n\tsetne cl\n\tmov eax,ecx\n"
						| OArrCrt -> (
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
					 	| OArrRead -> "\tmov eax,dword [eax+4*ebx]\n"
					 *)
					 	| _ -> raise (Failure (Printf.sprintf "Operation %s is not unfloat binary operation" os))
					)
				) ^
				"; " ^ (nd2ds nrd) ^ " ::= " ^ os ^ " " ^ (nd2ds nad) ^ " " ^ (nd2ds nbd) ^ "\n"
			)
		| OpOpr(nrd,op,[nad;nbd;ncd]) -> (
				let os = op2str op in
				(triopr2s nrd nad nbd ncd (
					match op with
				(*
					| OArrWrite -> "\tmov dword [eax+4*ebx],ecx\n"
				*)
					| _ -> raise (Failure (Printf.sprintf "Operation %s is not trinary operation" os))
				)) ^
				"; " ^ (nd2ds nrd) ^ " ::= " ^ os ^ " " ^ (nd2ds nad) ^ " " ^ (nd2ds nbd) ^ " " ^ (nd2ds ncd) ^ "\n"
			)
		| OpOpr(_,x,vs) -> raise (Failure (Printf.sprintf "Operation %s with %d argument in not defined yet" (op2str x) (List.length vs)))
	) ops))

(*
 nasm out.s -f elf32 -g -o out.o; gcc -m32 out.o
 nasm out.s -f win32 -g -o out.o; gcc -m32 out.o

*)

let vir2asm (funs,rd) = 
	"section .data\n" ^
	!constfs ^
	"section .bss\n" ^
	"global_heap:\n" ^
	"\tresb 0x80000000\n" ^
	"section .text\n" ^
	"global " ^ main_name ^ "\n" ^ 
	(String.concat "" (List.map func2asm (List.rev funs))) ^	
	(func2asm ((main_name,TyVar(-1)),[],[],rd))




