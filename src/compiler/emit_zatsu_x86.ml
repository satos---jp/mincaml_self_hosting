open Virtual
open Syntax
open Type
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

(*
型多相が問題になるのは、Equalityの比較の際。
データ型として、type_checker.mli に依ると
1. int
2. float
3. string
4. Num
5. var
6. arr
7. fun
8. tuple
9. UserDef

の9種類がある。
5. 6. 7. は実行時エラーでよさそう。
9. は実際はtupleになっている
1.2.3.4. は型によって比較方法が違う
8.9. は再帰的に比較したい。(9は諦めるかも？)

アドレスの上位4bitくらいは使えるはず
(関数は 0x08049000 にたいてい配置されるので)
なので、 int, float, string, tuple に関しては、そこにデータを載せる。

floatは載せられんやんけ... -> 下4bitshiftして、精度の下いくらかを用いる!!とよさそう。
-> intは負だと全1なのでだめですね...? -> 下1を使う。
int,float :: x100
float実際 :: ......... 100
string    :: x010
tuple     :: x001   で、下にはtupleの長さを入れるようにする。
*)


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


返り値はeaxに

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
	let untagt s = 
		(Printf.sprintf "\tand %s,0x8fffffff\n" s)
	in
	let untags s = 
		(Printf.sprintf "\tand %s,0x8fffffff\n" s)
	in
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
				(untagt "eax") ^
				("\tadd eax,4\n") ^
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
	
	(* 11.. なら 0011 をかける *)
	(* 01.. なら 0100 をかける *)
	let untagi s = 
		"\tmov edx,0x80000000\n" ^
		(Printf.sprintf "\tand edx,%s\n" s) ^
		"\tshr edx,1\n" ^
		(Printf.sprintf "\txor %s,edx\n" s) ^
		(Printf.sprintf "\txor %s,0x40000000\n" s) ^
		"\tshr edx,1\n" ^
		(Printf.sprintf "\txor %s,edx\n" s) ^
		"\tshr edx,1\n" ^
		(Printf.sprintf "\txor %s,edx\n" s)
	in
	let tagi s = 
		(Printf.sprintf "\tand %s,0x8fffffff\n" s) ^
		(Printf.sprintf "\tor  %s,0x40000000\n" s)
	in
	let tags s = 
		(Printf.sprintf "\tand %s,0x8fffffff\n" s) ^
		(Printf.sprintf "\tor  %s,0x20000000\n" s)
	in
	let tagt s = 
		(Printf.sprintf "\tand %s,0x8fffffff\n" s) ^
		(Printf.sprintf "\tor  %s,0x10000000\n" s)
	in
	let untag_tor ((x,(t,_)) as na) s = 
		ivprint (Printf.sprintf "untag %s with type %s\n" (namestr2str !x) (type2str t));
		(Printf.sprintf "\tmov %s,%s\n" s (nd2ps na)) ^
		(if t = TyInt then
			(untagi s)
		else if (match t with TyStr | TyTuple(_) | TyUserDef(_) -> true | _ -> false)  then
			(untags s)
		else "")
	in
	let tag_frr ((_,(t,_)) as na) s = 
		(if t = TyInt then
			(tagi s)
		else if t = TyStr then
			(tags s)
		else if (match t with TyTuple(_) | TyUserDef(_) -> true | _ -> false) then
			(tagt s)
		else "") ^
		(Printf.sprintf "\tmov %s,%s\n" (nd2ps na) s)
	in
	
	let untagf_fld na = 
		(Printf.sprintf "\tmov eax,%s\n" (nd2ps na)) ^
		(untagi "eax") ^
		"\trol eax,4\n" ^
		"\tpush eax\n" ^
		"\tfld dword [esp]\n" ^
		"\tpop eax\n"
	in
	let tagf s = 
		(Printf.sprintf "\tror %s,4\n" s) ^
		(tagi s)
	in
	let tagf_fstp na = 
		(Printf.sprintf "\tfstp %s\n" (nd2ps na)) ^
		(Printf.sprintf "\tmov eax,%s\n" (nd2ps na)) ^
		(tagf "eax") ^
		(Printf.sprintf "\tmov %s,eax\n" (nd2ps na))
	in
	
	let unopr2s nr na s = 
		(untag_tor na "eax") ^
		s ^ 
		(tag_frr nr "eax")
	in
	let biopr2s nr na nb s = 
		(untag_tor nb "ebx") ^
		(unopr2s nr na s)
	in
	let fbiopr2s nr na nb s = 
		(untagf_fld na) ^
		(untagf_fld nb) ^
		s ^ 
		(tagf_fstp nr)
	in
	let fcmpopr2s nr na nb s = 
		(untagf_fld na) ^
		(untagf_fld nb) ^
		s ^ 
		(tag_frr nr "eax")
		in
	let triopr2s nr na nb nc s = 
		(untag_tor nc "ecx") ^
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
	let check_data_eq s t =
		(Printf.sprintf "\tpush %s\n" s) ^ 
		(Printf.sprintf "\tpush %s\n" t) ^ 
		"\tcall data_eq\n" ^
		"\tadd esp,8\n"
	in
	fn ^ ":\n" ^ prologue ^ 
	(String.concat "" (List.map (fun op -> 
		match op with
		| OpMovi((na,(t,d)),CInt(v)) -> (
				assert (match t with TyInt -> true | _ -> false);
				(Printf.sprintf "\tmov eax,%d\n" v) ^ 
				(tagi "eax") ^
				(Printf.sprintf "\tmov %s,eax\n" (na2s na)) ^ 
				"; " ^ (debug_data2simple d) ^ "\n"
			)
		| OpMovi(((na,(t,d)) as nad),CFloat(v)) -> assert (t=TyFloat); (
				let tag = gen_const () in
				consts := (!consts) ^ (Printf.sprintf "%s:\n\tdd %f\n" tag v);
				(Printf.sprintf "\tmov eax,[%s]\n" tag) ^
				(tagf "eax") ^
				(Printf.sprintf "\tmov %s,eax\n" (na2s na)) ^
				"; " ^ (debug_data2simple d) ^ "\n"
			)
		| OpMovi((na,(t,d)),CString(v)) -> assert (t=TyStr); (
				let tag = gen_const () in
					consts := (!consts) ^ (Printf.sprintf "%s:\n\tdd %d\n\tdb \"%s\"\n" tag (String.length v) v);
					(Printf.sprintf "\tmov eax,%s\n" tag) ^
					(tags "eax") ^
					Printf.sprintf "\tmov %s,eax\n" (na2s na)
			)
		| OpMovi((na,(t,d)),CChar(v)) -> assert (t=TyChar); (
				let tag = gen_const () in
				(Printf.sprintf "\tmov eax,%d\n" (Char.code v)) ^ 
				(tagi "eax") ^
				(Printf.sprintf "\tmov %s,eax\n" (na2s na)) ^ 
				"; " ^ (debug_data2simple d) ^ "\n"
			)
		| OpMov(((n1,(t1,d1)) as nrd),((n2,(t2,d2)) as nad)) -> (
				(* TODO(satos) ここ不正確なので型スキームの同一性判定でやりたい。 *)
				if not (is_subtype t1 t2 && is_subtype t2 t1) then raise (Failure (Printf.sprintf 
					"Type mismatch move to %s (%s) : %s from %s (%s) : %s" 
					(namereg2str nrd) (debug_data2simple d1) (type2str t1) 
					(namereg2str nad) (debug_data2simple d2) (type2str t2))) else
				(mova2b nrd nad) 
			)
		| OpLabel x -> x ^ ":\n"
		| OpJcnd(ct,(na,_),(nb,_),la) -> (
				(Printf.sprintf "\tmov eax,%s\n\tmov ebx,%s\n" (na2s na) (na2s nb)) ^ 
				(match ct with
				 | CmpLt -> (Printf.sprintf "\tcmp eax,ebx\n\tjl %s\n" la)
				 | CmpEq -> (
				 		(check_data_eq "eax" "ebx") ^
				 		"\ttest eax,eax\n" ^
				 		(Printf.sprintf "\tjz %s\n" la)
				 	)
				)
			)
		| OpJmp(la) -> Printf.sprintf "\tjmp %s\n" la
	(*
		これ通らなくなったみたいなんで一旦消しとく
		TODO タグ外しやる
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
	*)
		| OpMakeTuple(nad,vs) -> (
				(Printf.sprintf "\tmov %s,esi\n" (nd2ps nad)) ^ 
				(Printf.sprintf "\tmov dword [esi],%d\n" (List.length vs)) ^ 
				"\tadd esi,4\n" ^
				(make_vs_on_heap vs) ^ 
				(Printf.sprintf "\tmov eax,%s\n" (nd2ps nad)) ^
				(tagt "eax") ^ 
				(Printf.sprintf "\tmov %s,eax\n" (nd2ps nad)) ^
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
					| OGetTuple(i) | OGetTupleWithLen(_,i) -> (Printf.sprintf "\tadd eax,%d\n\tmov eax,[eax]\n" (i*4+4))
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
					| Olt | Oleq | Ogt | Ogeq when (isi || isf) -> (
						(if isf then fcmpopr2s else biopr2s) nrd nad nbd (
							"\txor ecx,ecx\n" ^
							(if isf then "\tfcomip\n" else "\tcmp eax,ebx\n") ^ 
							(if isf then (
								match opr with
								| Olt  -> "\tseta cl\n"
								| Oleq -> "\tsetae cl\n"
								| Ogt  -> "\tsetb cl\n"
								| Ogeq -> "\tsetbe cl\n"
								| _ -> raise (Failure (Printf.sprintf "ocmp swith shouldn't reach here"))
							) else (
								match opr with
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
					| Oeq -> (
							(Printf.sprintf "\tmov eax,%s\n" (nd2ps nad)) ^
							(Printf.sprintf "\tmov ebx,%s\n" (nd2ps nbd)) ^
							(check_data_eq "eax" "ebx") ^
							(tagi "eax") ^
							(Printf.sprintf "\tmov %s,eax\n" (nd2ps nrd))
					)
					| Oneq -> (
							(Printf.sprintf "\tmov eax,%s\n" (nd2ps nad)) ^
							(Printf.sprintf "\tmov ebx,%s\n" (nd2ps nbd)) ^
							(check_data_eq "eax" "ebx") ^
							"\txor eax,1\n" ^
							(tagi "eax") ^
							(Printf.sprintf "\tmov %s,eax\n" (nd2ps nrd))
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


let vir2asm (funs,rd,globvars) externs exports fn = 
	on_exports := (init_exs exports);
	on_externs := (init_exs (List.map fst externs));
	let start_name = fn ^ "main" in
	main_name_str := start_name;
	"BITS 32\n" ^
	(String.concat "" (List.map (fun (s,_) -> 
		"extern " ^ s ^ "\n"
	) externs)) ^
	"extern global_heap\n" ^
	"extern data_eq\n" ^
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



