open Virtual
open Syntax
open Type_checker
open Debug
open Linux_win_diff
open Debug
open Main_option
open Genint
open Op
(* とりあえず、雑にx86コードを生成する *)

let consts = ref ""

let gen_const () = Printf.sprintf "@const_%d" (genint ())

let genlabel () = Printf.sprintf "@emit_label_%d" (genint ())



let vs2stacks vs = 
	let rec f (ar,sl) vs = 
		match vs with
		| [] -> (ar,sl)
		| (na,ty) :: xs -> 
			let nl = 4
				(* 関数は(クロージャへのポインタ,関数へのポインタ)で持ち、それ以外は1つで 
					 と思ったが、あまりにも面倒なので、ヒープに持ちます *)
			in
			let tna = ( 
				match !na with
				| Var x | GVar x -> x
				| Reg _ -> raise (Failure "register allocation is not supported int x86") 
			)
			in
				f ((tna,sl) :: ar,nl+sl) xs
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
*)

let on_glob_vars = ref []
let heap_diff = ref 0
let init_globvars gvs = 
	on_glob_vars := List.fold_left (fun r -> fun x -> 
		heap_diff := !heap_diff+4;
		(x,!heap_diff-4) :: r
	) [] gvs


let on_exports = ref []
let on_externs = ref []

let init_exs vs = 
	fst (
		List.fold_left (fun (r,d) -> fun x -> 
			((x,d) :: r,d+4)
		) ([],0) vs
	)

let main_name_str = ref ""
let main_name () = !main_name_str

let func2asm {fn=(fn,_); vs=vs1; cvs=vs2; body={ops=ops; vs=localvs}} = 
	(*
	Printf.printf "%s%s%s" (names2str vs1) (names2str vs2) (names2str localvs);
	*)
	let lvs_st = vs2stacks localvs in
	let vs1_st = vs2stacks vs1 in
	let vs2_st = vs2stacks vs2 in
	
	let on_stack = 
		(List.map (fun (x,p) -> (x,p-(snd lvs_st))) (fst lvs_st)) @
		(List.map (fun (x,p) -> (x,p+8)) (fst vs1_st)) in
	let on_clos = fst vs2_st in
	ivprint ("On function " ^ fn ^ "\n");
	ivprint ((String.concat "\n" (List.map (fun (s,p) -> Printf.sprintf "%s :: [ebp%+d]" s p) on_stack)) ^"\n");
	ivprint ((String.concat "\n" (List.map (fun (s,p) -> Printf.sprintf "%s :: [edi%+d]" s p) on_clos)) ^"\n");
	let na2pt na = (
		match !na with
		| Var x -> (
			try ("ebp",List.assoc x on_stack)
			with | Not_found -> 
			try ("edi",List.assoc x on_clos)
			with | Not_found -> 
			raise (Failure "var should be on the stack or closure")
		)
		| GVar x -> (
			try ("global_heap",List.assoc x !on_glob_vars)
			with | Not_found -> 
			try (
				(* 高速化っぽいことができるはず *)
				(* いや、無理で、 let f x = x と let g = f を区別できなそう*)
				(* ("extern_list",List.assoc x !on_externs) こんなまどろまなくても *)
				let _ = ("extern_list",List.assoc x !on_externs) in
				(x,0)
			)
			with | Not_found -> 
			("#" ^ x,-1)
		)
		| Reg _ -> raise (Failure "Register allocation for x86 is not implemented yet")
	) in
	let pt2s (a,b) = 
		if String.get a 0 = '#' then String.sub a 1 ((String.length a)-1) else 
			Printf.sprintf "dword [%s%+d]" a b 
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
				(Printf.sprintf "\tmov eax,%s\n\tmov dword [esi%+d],eax\n" (pt2s (p,l)) (!nl-4))
			) vs)) in
		cs ^ (Printf.sprintf "\tadd esi,%d\n" !nl)
	) in
	let eprintc x = (
		(Printf.sprintf "\tpush %d\n" x) ^
		"\tcall print_char_err\n" ^
		"\tadd esp,4\n"
	) in
	let prologue = 
		(Printf.sprintf "\tpush ebp\n\tmov ebp,esp\n\tsub esp,%d\n" (snd lvs_st)) ^
		(if fn = main_name () then (
			(*
			(String.concat "" (List.map (fun (na,p) -> 
				(Printf.sprintf "\tmov eax,[%s]\n" na) ^ 
				(Printf.sprintf "\tmov [extern_list+%d],eax\n" p)
			) !on_externs)) ^ *)
			(Printf.sprintf "\tadd esi,%d\n" !heap_diff)  
		)else "")
	in
	let epilogue = 
		(if fn = main_name () then (
				(String.concat "" (List.map (fun (_,p) -> 
					"\tmov ebx,[eax]\n" ^ 
					(Printf.sprintf "\tmov [export_list+%d],ebx\n" p) ^ 
					"\tadd eax,4\n"
				) (List.rev !on_exports)))
		) else "") ^
		(Printf.sprintf "\tadd esp,%d\n\tpop ebp\n\tret\n" (snd lvs_st))
	in
	
	let mova2b nad nbd = 
			(Printf.sprintf "\tmov eax,%s\n" (nd2ps nbd)) ^
			(Printf.sprintf "\tmov %s,eax\n" (nd2ps nad)) ^
			"; " ^ (nd2ds nad) ^ " ::<= " ^ (nd2ds nbd) ^ "\n"
	in
	let unopr2s nr na s = 
		(Printf.sprintf "\tmov eax,%s\n" (nd2ps na)) ^
		s ^ 
		(Printf.sprintf "\tmov %s,eax\n" (nd2ps nr))
	in
	let biopr2s nr na nb s = 
		(Printf.sprintf "\tmov ebx,%s\n" (nd2ps nb)) ^
		(unopr2s nr na s)
	in
	let fbiopr2s nr na nb s = 
		(Printf.sprintf "\tfld %s\n" (nd2ps na)) ^
		(Printf.sprintf "\tfld %s\n" (nd2ps nb)) ^
		s ^ 
		(Printf.sprintf "\tfstp %s\n" (nd2ps nr))
	in
	let fcmpopr2s nr na nb s = 
		(Printf.sprintf "\tfld %s\n" (nd2ps na)) ^
		(Printf.sprintf "\tfld %s\n" (nd2ps nb)) ^
		s ^ 
		(Printf.sprintf "\tmov %s,eax\n" (nd2ps nr))
	in
	let triopr2s nr na nb nc s = 
		(Printf.sprintf "\tmov ecx,%s\n" (nd2ps nc)) ^
		(biopr2s nr na nb s)
	in
	let print_errstr s = 
		let ls = String.length s in
		let tag = gen_const () in
		consts := (!consts) ^ (Printf.sprintf "%s:\n\tdb \"%s\"\n" tag s);
		(Printf.sprintf "\tmov eax,%d\n\tpush eax\n" ls) ^ 
		(Printf.sprintf "\tmov eax,%s\n\tpush eax\n" tag) ^ 
		"\tcall puts_err\n\tadd esp,8\n"
	in
	let abort d = 
		(print_errstr ("Abort " ^ (debug_data2str d) ^ " ")) ^
		"\tint 0x3\n"	
	in
	let check_boundary d = 
		let la = genlabel () in
		let lb = genlabel () in
		"\tcmp ebx,0\n" ^
		(Printf.sprintf "\tjl %s\n" la) ^
		"\tcmp ebx,dword [eax]\n" ^
		(Printf.sprintf "\tjl %s\n" lb) ^
		(* チェック失敗 *)
		(Printf.sprintf "%s:\n" la) ^ 
		(abort d) ^
		(* チェック成功 *)
		(Printf.sprintf "%s:\n" lb) ^ 
		"\tadd eax,4\n"
	in
	fn ^ ":\n" ^ prologue ^ 
	(String.concat "" (List.map (fun op -> 
		match op with
		| OpMovi((na,(t,d)),CInt(v)) -> assert (match t with TyInt | TyUserDef _ -> true | _ -> false); 
			(Printf.sprintf "\tmov %s,%d\n" (na2s na) v) ^ "; " ^ (debug_data2simple d) ^ "\n"
		| OpMovi((na,(t,d)),CFloat(v)) -> assert (t=TyFloat); (
				let tag = gen_const () in
					consts := (!consts) ^ (Printf.sprintf "%s:\n\tdd %f\n" tag v);
					Printf.sprintf "\tmov eax,[%s]\n\tmov %s,eax\n" tag (na2s na)
			)
		| OpMovi((na,(t,d)),CString(v)) -> assert (t=TyString); (
				let tag = gen_const () in
					consts := (!consts) ^ (Printf.sprintf "%s:\n\tdd %d\n\tdb \"%s\"\n" tag (String.length v) v);
					Printf.sprintf "\tmov eax,%s\n\tmov %s,eax\n" tag (na2s na)
			)
		| OpMov(((n1,(t1,d1)) as nrd),((n2,(t2,d2)) as nad)) -> (
				if t1 <> t2 then raise (Failure (Printf.sprintf 
					"Type mismatch move to %s (%s) : %s from %s (%s) : %s" 
					(namereg2str nrd) (debug_data2simple d1) (type2str t1) 
					(namereg2str nad) (debug_data2simple d2) (type2str t2))) else
				(mova2b nrd nad) 
			)
		| OpLabel x -> x ^ ":\n"
		| OpJcnd(ct,(na,_),(nb,_),la) -> (
				(Printf.sprintf "\tmov eax,%s\n\tmov ebx,%s\n" (na2s na) (na2s nb)) ^ 
				(Printf.sprintf "\tcmp eax,ebx\n\t%s %s\n" (match ct with CmpEq -> "jne" | CmpLt -> "jl") la)
			)
		| OpJmp(la) -> Printf.sprintf "\tjmp %s\n" la
		| OpDestTuple(vs,nad) -> (
				let nl = ref 0 in
				(Printf.sprintf "\tmov eax,%s\n" (nd2ps nad)) ^ 
				(String.concat "" (List.map (fun (na,nt) -> 
					let (p,l) = na2pt na in
						nl := !nl + 4;
						(Printf.sprintf "\tmov ebx,dword [eax%+d]\n\tmov %s,ebx\n" (!nl-4) (pt2s (p,l)))
					) vs)) ^
				"; " ^ (nd2ds nad) ^ "\n"
			)
		| OpMakeTuple(nad,vs) -> (
				(Printf.sprintf "\tmov %s,esi\n" (nd2ps nad)) ^ 
				(make_vs_on_heap vs) ^ 
				"; " ^ (nd2ds nad) ^ "\n"
			)
		| OpMakeCls(nad,((fn,_) as fnd),vs) -> (
				"\tmov edx,esi\n" ^ 
				(make_vs_on_heap vs) ^
				"\tmov dword [esi+4],edx\n" ^ 
				(Printf.sprintf "\tmov dword [esi],%s\n" fn) ^ 
				(Printf.sprintf "\tmov %s,esi\n" (nd2ps nad)) ^ 
				"\tadd esi,8\n"^
				"; " ^ (nd2ds nad) ^ " "^ (nd2ds fnd) ^ "\n"
			)
		| OpApp(istail,isdir,nad,((fn,_) as fnd),vs) -> (
				let rfn = (na2s fn) in
				(*
				Printf.printf "funname %s -> %s\n" (namereg2str fnd) rfn;
				*)
				let isglobal = List.mem rfn (global_funcs ()) in
				(* x86のtail化はやめておく *)
				let isdir = match isdir with DirApp -> true | InDirApp -> false in
				let ln = ref 0 in
				let s = 
				"\tpush edi\n" ^
				(String.concat "" (List.map (fun nad -> 
					ln := !ln + 4;
					(Printf.sprintf "\tpush dword %s\n" (nd2ps nad))
				) (List.rev vs))) ^ (* 逆にpushする *)
				(if isglobal || isdir then 
					(Printf.sprintf "\tcall %s\n" rfn)
				else 
					(Printf.sprintf "\tmov eax,%s\n" rfn) ^ 
					"\tmov edi,[eax+4]\n" ^ 
					(Printf.sprintf "\tcall [eax]\n")) ^
				(Printf.sprintf "\tmov %s,eax\n" (nd2ps nad))
				in s ^ 
				(Printf.sprintf "\tadd esp,%d\n" !ln) ^
				"\tpop edi\n"
				^
				"; " ^ (nd2ds nad) ^ " " ^ (nd2ds fnd) ^ "\n" ^ 
				"; " ^ (Printf.sprintf "%s : %s" (if isdir then "dir" else "indir") (if false then "tail" else "nontail")) ^ "\n"
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
		| OpOpr(nrd,Osemi2,[_;nbd]) -> mova2b nrd nbd
		| OpOpr(nrd,Osemi1,[nad]) -> mova2b nrd nad
		| OpOpr(nrd,op,[nad]) -> (
				let os = op2str op in
				(unopr2s nrd nad (
					match op with
					| Ominus -> "\tneg eax\n"
					| Onot   -> "\ttest eax,eax\n\tsete al\n\tand eax,1\n"
					| OGetTuple(i) -> (Printf.sprintf "\tadd eax,%d\n\tmov eax,[eax]\n" (i*4))
					| Oiadd(x) -> (Printf.sprintf "\tadd eax,%d\n" x)
					| Oibysub(x) -> (Printf.sprintf "\tsub eax,%d\n" x)
					| Oimul(x) -> (Printf.sprintf "\tmov edx,%d\n\tmul edx\n" x)
					| Oibydiv(x) -> (Printf.sprintf "\txor edx,edx\n\tmov ebx,%d\n\tdiv ebx\n" x)
					| OiArrRead(x) -> Printf.sprintf "\tmov eax,dword [eax+%d]\n" (x*4)
					| _ -> raise (Failure (Printf.sprintf "Operation %s is not unary operation" os))
			 	)) ^
				"; " ^ (nd2ds nrd) ^ " ::= " ^ os ^ " " ^ (nd2ds nad) ^ "\n"
			)
		| OpOpr(nrd,opr,[nad;nbd]) -> (
				let os = op2str opr in
				(if List.mem opr [Ofadd;Ofsub;Ofmul;Ofdiv] then (
					fbiopr2s nrd nad nbd (
						match opr with
						| Ofadd -> "\tfaddp\n"
						| Ofsub -> "\tfsubp\n"
						| Ofmul -> "\tfmulp\n"
						| Ofdiv -> "\tfdivp\n"
						| _ -> raise (Failure (Printf.sprintf "Operation %s is not float binary operation" os))
					) 
				) else (
					(* 多相性のため。もっと型チェックを入れてもいいかもしれん *)
					let isf = ((fst (snd nad)) = TyFloat) in
					let isi = ((fst (snd nad)) = TyInt) in
					match opr with 
					| Oeq | Oneq | Olt | Oleq | Ogt | Ogeq when (isi || isf) -> (
						(if isf then fcmpopr2s else biopr2s) nrd nad nbd (
							"\txor ecx,ecx\n" ^
							(if isf then "\tfcomip\n" else "\tcmp eax,ebx\n") ^ 
							(if isf then (
								match opr with
								| Oeq  -> "\tsete cl\n"
								| Oneq -> "\tsetne cl\n"
								| Olt  -> "\tseta cl\n"
								| Oleq -> "\tsetae cl\n"
								| Ogt  -> "\tsetb cl\n"
								| Ogeq -> "\tsetbe cl\n"
								| _ -> raise (Failure (Printf.sprintf "ocmp swith shouldn't reach here"))
							) else (
								match opr with
								| Oeq  -> "\tsete cl\n"
								| Oneq -> "\tsetne cl\n"
								| Olt  -> "\tsetl cl\n"
								| Oleq -> "\tsetle cl\n"
								| Ogt  -> "\tsetg cl\n"
								| Ogeq -> "\tsetge cl\n"
								| _ -> raise (Failure (Printf.sprintf "ocmp swith shouldn't reach here"))
							 )) ^
							 (if isf then "\tpush eax\n\tfstp dword [esp]\n\tpop eax\n\tfcomip\n" else "") ^
							 "\tmov eax,ecx\n"
						)
					)
					| Oeq | Oneq -> (
							raise (Failure (Printf.sprintf "TODO"))
						)
					| _ -> (
						biopr2s nrd nad nbd ( 
							match opr with
							| Oadd -> "\tadd eax,ebx\n"
							| Osub -> "\tsub eax,ebx\n"
							| Omul -> "\tmul ebx\n"
							| Odiv -> "\txor edx,edx\n\tdiv ebx\n"
						 	| OArrCrt -> (
									let la = genlabel () in
									let lb = genlabel () in
									"\tmov ecx,esi\n" ^
									(if !check_array_boundary then (
										"\tmov dword [esi],eax\n" ^ 
										"\tadd esi,4\n"
									) else "") ^
									"\ttest eax,eax\n" ^
									(Printf.sprintf "\tje %s\n" lb) ^
									(Printf.sprintf "%s:\n" la) ^ 
									"\tmov dword [esi],ebx\n" ^
									"\tadd esi,4\n" ^
									"\tsub eax,1\n" ^
									(Printf.sprintf "\tjne %s\n" la) ^
									(Printf.sprintf "%s:\n" lb) ^ 
									"\tmov eax,ecx\n"
						 		)
						 	| OArrRead -> (
						 			let (_,(_,d)) = nrd in
						 			(if !check_array_boundary then check_boundary d else "") ^ 
						 			"\tmov eax,dword [eax+4*ebx]\n"
						 		)
							| OiArrWrite(x) -> Printf.sprintf "\tmov dword [eax+%d],ebx\n" (x*4)
						 	| _ -> raise (Failure (Printf.sprintf "Operation %s is not unfloat binary operation" os))
						) 
					) 
				)) ^
				"; " ^ (nd2ds nrd) ^ " ::= " ^ os ^ " " ^ (nd2ds nad) ^ " " ^ (nd2ds nbd) ^ "\n"
			)
		| OpOpr(nrd,opr,[nad;nbd;ncd]) -> (
				let os = op2str opr in
				(triopr2s nrd nad nbd ncd (
					match opr with
					| OArrWrite -> (
							let (_,(_,d)) = nrd in
						 	(if !check_array_boundary then check_boundary d else "") ^ 
						 	"\tmov dword [eax+4*ebx],ecx\n"
						)
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


let vir2asm (funs,rd,globvars) externs exports = 
	on_exports := (init_exs exports);
	on_externs := (init_exs (List.map fst externs));
	let start_name = !filename ^ "main" in
	main_name_str := start_name;
	"BITS 32\n" ^
	(String.concat "" (List.map (fun (s,_) -> 
		"extern " ^ s ^ "\n"
	) externs)) ^
	"extern global_heap\n" ^
	(String.concat "" (List.map (fun s -> 
		"global " ^ s ^ "\n"
	) exports)) ^
	
	"section .data\n" ^
	!consts ^
	"export_list:\n" ^
	(String.concat "" (List.map (fun s -> 
		(Printf.sprintf "%s:\n" s) ^
		"\tdd 0\n"
	) exports)) ^
	
	"section .bss\n" ^
	"extern_list:\n" ^
	(Printf.sprintf "\tresb %d\n" ((List.length externs) * 4)) ^
	
	"section .text\n" ^
	"global " ^ start_name ^ "\n" ^ 
	(
		init_globvars globvars;
		let f s = if !debugmode then add_inscount s else s in
		(String.concat "" (List.map (fun x -> f (func2asm x)) (List.rev funs))) ^
		(f (func2asm {fn=(start_name,(TyVar(-1),default_debug_data)); vs=[]; regs=[]; cvs=[]; body=rd}))
	)



