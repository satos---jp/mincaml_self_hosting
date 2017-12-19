open Virtual
open Syntax
open Type_checker
open Debug
open Genint
open Op
open Str
(* とりあえず、雑に tortesia コードを生成する *)

let constfs = ref ""

let gen_const () = Printf.sprintf "@const_%d" (genint ())

let genlabel () = Printf.sprintf "@emit_label_%d" (genint ())


(*
[ebp-0x4] 以降 .. ローカル変数
[ebp] .. esp
[ebp+0x4] .. retaddr
[ebp+0x8] 以下 .. 引数
で。


とりあえず、

r1 :: esp
r2 :: ebp
r3 :: esi (ヒープポインタ)
r4 :: edi (クロージャへのポインタ)

r31 :: グローバルヒープへのポインタ
とします。

関数引数 r30 ... r10, f31 .. f10 をそれぞれこの順に。余ればスタックに。
一時変数 r5 ... r7, f1,f2 。
その他は全て、とりあえずcallee save でやっていく。

*)

(* globalな変数のヒープ上の初期化をしておく *)
let on_heap_vars = ref []
let heap_diff = ref 0
let init_heapvars gvs = 
	on_heap_vars := List.fold_left (fun r -> fun x -> 
		heap_diff := !heap_diff+4;
		(x,!heap_diff-4) :: r
	) [] gvs

let main_name = "main"

let vs2stacks vs = 
	let rec f (ar,sl) vs = 
		match vs with
		| [] -> (ar,sl)
		| (na,ty) :: xs -> 
			let nl = 4
			in
				f ((na,sl) :: ar,nl+sl) xs
	in
		f ([],0) vs

(*
名前には 5 種類あり、
・引数変数
・クロージャ変数
・ローカル変数
	(virtualにおいて、opから、get_var_names_from_opsで取られる)
	(名前のうち、closureで得られた関数名 と global_funcs(type_checkerで定義されてるやつ) を除いたもの)
・グローバル変数なやつ(closure_conv で toplevelか判断してglobvarsに載せる)(emit において on_glob_vars に載る)
・関数のラベルのやつ(名前のうち、上の4つでないやつ。)
となっている。

このうち、
ローカル変数はスタックに乗るか、レジスタであり、
グローバル変数なやつ はヒープから取られることになり、
関数のラベルなやつ、は名前がそのまま書かれる。

レジスタ割り当てされるのは、ローカル変数のみ。
*)

let func2asm {fn=(fn,_); vs=vs1; cvs=vs2; body={ops=ops; vs=localvs}} = 
	(*
	Printf.printf "%s%s%s" (Closure_conv.vs2str vs1) (Closure_conv.vs2str vs2) (Closure_conv.vs2str localvs);
	*)
	let lvs_st = vs2stacks localvs in
	let vs1_st = vs2stacks vs1 in
	let vs2_st = vs2stacks vs2 in
	
	let on_stack = 
		(List.map (fun (x,p) -> (x,p-(snd lvs_st))) (fst lvs_st)) @
		(List.map (fun (x,p) -> (x,p+8)) (fst vs2_st)) in
	let on_clos = fst vs1_st in
	print_string ("On function " ^ fn ^ "\n");
	print_string ((String.concat "\n" (List.map (fun (s,p) -> Printf.sprintf "%s :: [r2$%d]" s p) on_stack)) ^"\n");
	print_string ((String.concat "\n" (List.map (fun (s,p) -> Printf.sprintf "%s :: [r4$%d]" s p) on_clos)) ^"\n");
	let na2pt x = (
		try ("r2",List.assoc x on_stack)
		with | Not_found -> 
		try ("r4",List.assoc x on_clos)
		with | Not_found -> 
		try ("r31",List.assoc x !on_heap_vars) (* 正直ガバなのでどうにかしたい *)
		with | Not_found -> 
		("@" ^ x,-1)
	) in
	let pt2s (a,b) = 
		if String.get a 0 = '@' then String.sub a 1 ((String.length a)-1) else 
			Printf.sprintf "%s,$%d" a b 
	in
	let na2s x = pt2s (na2pt x) in
	let ls2r ls r (na,_) = (
		let res,rg = 
		match !na with
		| Var x | GVar x -> (Printf.sprintf "\t%s %s,%s\n" ls r (na2s x)), r
		| Reg x -> "",x
		in
		res,rg
	) in
	let nd2ds (_,(_,d)) = (debug_data2simple d) in
	let make_vs_on_heap vs = (
		let nl = ref 0 in
		let cs = 
			(String.concat "" (List.map (fun na ->
				nl := !nl + 4;
				let s,r = ls2r "lw" "r5" na in
				s ^ 
				(Printf.sprintf "\tsw %s,r3,$%d\n" r (!nl-4))
			) vs)) in
		cs ^ (Printf.sprintf "\taddi r3,r3,$%d\n" !nl)
	) in
	(* let canary = genint () in *)
	let prologue = 
		"\tmflr r7\n" ^ 
		"\tpush r7\n" ^ 
		
		(if fn = main_name then Printf.sprintf "\tmov r31,r3\n\taddi r3,r3,$%d\n" !heap_diff else "") ^
		(Printf.sprintf "\tpush r2\n\tmov r2,r1\n\tsubi r1,r1,$%d\n" (snd lvs_st))
	in
	let epilogue = (
		(if fn = main_name then "\thlt\n" else 
		((Printf.sprintf "\taddi r1,r1,$%d\n\tpop r2\n" (snd lvs_st)) ^
		
 		"\tpop r6\n" ^ 
		"\tjr r6\n"))
	)
	in
	let mova2b nr na = 
		(match !(fst nr),!(fst na) with
		| Var r,Var a | Var r,GVar a | GVar r,Var a | GVar r,GVar a -> (fst (ls2r "lw" "r5" na)) ^ (fst (ls2r "sw" "r5" nr))
		| Reg r,Var a | Reg r,GVar a -> Printf.sprintf "\tlw %s,%s\n" r (na2s a)
		| Var r,Reg a | GVar r,Reg a -> Printf.sprintf "\tsw %s,%s\n" (na2s r) a
		| Reg r,Reg a -> Printf.sprintf "\tmov %s,%s\n" r a
		) ^		"; " ^ (nd2ds na) ^ " ::<= " ^ (nd2ds nr) ^ "\n"
	in
	(* この辺、かっこよくしたい *)
	let myprint0 s r = 
		Str.global_replace (Str.regexp "{0}") r s
	in
	let myprint1 s r a =
		myprint0 (Str.global_replace (Str.regexp "{1}") a s) r
	in
	let myprint2 s r a b = 
		myprint1 (Str.global_replace (Str.regexp "{2}") b s) r a
	in
	let myprint3 s r a b c = 
		myprint2 (Str.global_replace (Str.regexp "{3}") c s) r a b
	in
	let unopr2s nr na s = 
		let s1,r1 = ls2r "lw" "r5" na in
		let s2,r2 = ls2r "sw" "r5" nr in
		s1 ^ (myprint1 s r2 r1) ^ s2
	in
	let biopr2s nr na nb s = 
		let s1,r1 = ls2r "lw" "r5" na in
		let s3,r3 = ls2r "lw" "r6" nb in
		let s2,r2 = ls2r "sw" "r5" nr in
		s1 ^ s3 ^ (myprint2 s r2 r1 r3) ^ s2
	in
	let fbiopr2s nr na nb s = 
		let s1,r1 = ls2r "fld" "f1" na in
		let s3,r3 = ls2r "fld" "f2" nb in
		let s2,r2 = ls2r "fst" "f1" nr in
		s1 ^ s3 ^ (myprint2 s r2 r1 r3) ^ s2
	in
	let fcmpopr2s nr na nb s = 
		let s1,r1 = ls2r "fld" "f1" na in
		let s3,r3 = ls2r "fld" "f2" nb in
		let s2,r2 = ls2r "sw" "r5" nr in
		s1 ^ s3 ^ (myprint2 s r2 r1 r3) ^ s2
	in
	let triopr2s nr na nb nc s = 
		let s1,r1 = ls2r "lw" "r5" na in
		let s3,r3 = ls2r "lw" "r6" nb in
		let s4,r4 = ls2r "lw" "r7" nc in
		let s2,r2 = ls2r "sw" "r5" nr in
		s1 ^ s3 ^ s4 ^ (myprint3 s r2 r1 r3 r4) ^ s2
	in
	let v2reg tr na = 
		let s,r = ls2r "lw" tr na in
		if tr = r then s else (Printf.sprintf "mov %s,%s" tr r)
	in
	let reg2v na tr = 
		let s,r = ls2r "sw" tr na in
		if tr = r then s else (Printf.sprintf "mov %s,%s" r tr)		
	in
	fn ^ ":\n" ^ prologue ^ 
	(String.concat "" (List.map (fun op -> 
		match op with
		| OpMovi((_,(t,d)) as na,CInt(v)) -> (
				assert (t=TyInt); 
				let s,r = ls2r "sw" "r5" na in
				(Printf.sprintf "\tli %s,$%d\n" r v) ^ s ^
				"; " ^ (debug_data2simple d) ^ "\n"
			)
		| OpMovi((_,(t,d)) as na,CFloat(v)) -> (
				assert (t=TyFloat);
				let s,r = ls2r "fst" "f1" na in
				(Printf.sprintf "\tfmovi %s,$%.20f\n" r v) ^ s ^
				"; " ^ (debug_data2simple d) ^ "\n"
			)
		| OpMov(((na,(t1,d1)) as nrd),((nb,(t2,d2)) as nad)) -> assert (t1=t2); (mova2b nrd nad) 
		| OpLabel x -> x ^ ":\n"
		| OpJcnd(ct,na,nb,la) -> (
					let s1,r1 = ls2r "lw" "r5" na in
					let s2,r2 = ls2r "lw" "r6" nb in
					s1 ^ s2 ^ 
				(Printf.sprintf "\t%s %s,%s,%s\n" (match ct with 
					| CmpEq -> "bne"
					| CmpLt -> raise (Failure "unimplemented emit CmpLt at virtual.ml")) r1 r2 la)
			)
		| OpJmp(la) -> Printf.sprintf "\tj %s\n" la
		| OpDestTuple(vs,na) -> (
				let nl = ref (-4) in
				let s1,r1 = ls2r "lw" "r5" na in
				s1 ^ 
				(String.concat "" (List.map (fun nb -> 
					let s2,r2 = ls2r "sw" "r6" na in
					nl := !nl + 4;
					(Printf.sprintf "\tlw %s,%s,$%d\n" r2 r1 !nl) ^
					s2 
				) vs)) ^
				"; " ^ (nd2ds na) ^ "\n"
			)
		| OpMakeTuple(nr,vs) -> (
				(reg2v nr "r3") ^
				(make_vs_on_heap vs) ^ 
				"; " ^ (nd2ds nr) ^ "\n"
			)
		| OpMakeCls(nr,((fn,_) as fnd),vs) -> (
			(* closureは、[プログラムへのポインタ,引数] のこの順での組。 *)
				"\tmov r6,r3\n" ^ 
				(make_vs_on_heap vs) ^
				"\tsw r6,r3,$4\n" ^ 
				(Printf.sprintf "\tli r5,%s\n" fn) ^ 
				"\tsw r5,r3,$0\n" ^ 
				(reg2v nr "r3") ^
				"\taddi r3,r3,$8\n" ^
				"; " ^ (nd2ds nr) ^ " "^ (nd2ds fnd) ^ "\n"
			)
		| OpApp(istail,isdir,nr,((fn,_) as fnd),vs) -> (
				let nl = ref 0 in
				let s = 
				"\tpush r4\n" ^
				(String.concat "" (List.map (fun na -> 
					nl := !nl + 4;
					let s,r = ls2r "lw" "r5" na in
					s ^ 
					(Printf.sprintf "\tpush %s\n" r)
				) (List.rev vs))) ^ (* 逆にpushする *)
				(if (match !fn with GVar x when List.mem x (global_funcs ()) -> true | _ -> false) || isdir = DirApp then (
					match !fn with
					| GVar x -> Printf.sprintf "\tjal %s\n" x
					| _ -> (
						let s,r = ls2r "lw" "r5" fnd in
						s ^
						(Printf.sprintf "\tjal %s\n" r)
					)
				) else (
					let s,r = ls2r "lw" "r5" fnd in
					s ^
					(Printf.sprintf "\tlw r4,%s,$4\n" r) ^ 
					(Printf.sprintf "\tlw %s,%s,$0\n" r r) ^ 
					(Printf.sprintf "\tjalr %s\n" r)
				)) ^ 
				(reg2v nr "r5")
				in (* こうしないと、nlがアップデートされない *)
				s ^ 
				(Printf.sprintf "\taddi r1,r1,$%d\n" !nl) ^
				"\tpop r4\n" ^
				"; " ^ (nd2ds nr) ^ " " ^ (nd2ds fnd) ^ "\n"
			)
		| OpRet(na) -> (
				(v2reg "r5" na) ^
				epilogue
			)
		| OpMainRet -> (
				epilogue
			)
		| OpOpr(nrd,Osemi2,[_;nbd]) -> mova2b nrd nbd
		| OpOpr(nrd,Osemi1,[nad]) -> mova2b nrd nad
		| OpOpr(nrd,op,[nad]) -> (
				let rec tomul a = 
					(if a <= 0 then "" else (tomul (a/2))) ^
						"\tsll r6,r6,$1\n" ^
						(if (a mod 2 <> 0) then "\tadd r6,r6,{1}\n" else "")
				in
				unopr2s nrd nad (
					match op with
					| Ominus -> "\tsub {0},r0,{1}\n"
					| Onot -> "\tslti r6,{1},$0\n\tslti r7,{1},$1\n\tsub {0},r7,r6\n"
					| OGetTuple(i) -> Printf.sprintf "\tlw {0},{1},$%d\n" (i*4)
					| Oimul(x) -> (
							"\tli r6,$0\n" ^ (tomul x) ^ "\tmov {0},r6\n"
						)
					| Oibydiv(x) -> (
						match x with
						| 2 -> "\tslti r6,{1},$0\n\tadd {0},{1},r6\n\tsra {0},{0},$1\n"
						(* from https://stackoverflow.com/questions/5558492/divide-by-10-using-bit-shifts *)
						| 10 -> (
								"\tli r6,$0\n" ^ (tomul 205) ^ "\tsra {0},r6,$11\n"
							)
							(*
								"\tli r6,$0\n" ^ (tomul 205) ^ "\tsra r5,r6,$11\n"
								"\tli r6,$0\n" ^ (tomul 0x1999999) ^ "\tmov r5,r6\n"
							*)
						| _ -> raise (Failure (Printf.sprintf "divide by %d is not supported" x))
						)
					| _ -> raise (Failure (Printf.sprintf "Operation %s is not unary operation" (op2str op)))
				) ^
				"; " ^ (nd2ds nrd) ^ " ::= " ^ (op2str op) ^ " " ^ (nd2ds nad) ^ "\n"
			)
		| OpOpr(nrd,op,[nad;nbd]) -> (
				let os = op2str op in
				(*
				(print_string ((type2str (fst (snd nad)))^"\n"));
				(print_string (os ^ "\n"));
				*)
				(if (fst (snd nad)) = TyFloat then (
					(if List.mem op [Ofadd; Ofsub; Ofmul; Ofdiv] then (
						fbiopr2s nrd nad nbd (
							match op with
							| Ofadd -> "\tfadd {0},{1},{2}\n"
							| Ofsub -> "\tfsub {0},{1},{2}\n"
							| Ofmul -> "\tfmul {0},{1},{2}\n"
							| Ofdiv -> "\tfdiv {0},{1},{2}\n"
							| _ -> raise (Failure (Printf.sprintf "Operation %s is not float binary operation" os))
						)
					) else (
						let fcmpla (f1,f2) cs (t1,t2) = 
							let la = genlabel () in
							let lb = genlabel () in
							(Printf.sprintf "\t%s {%d},{%d}\n" cs f1 f2) ^ 
							(Printf.sprintf "\tbft %s\n" la) ^
							(Printf.sprintf "\tli {0},$%d\n\tj %s\n" t2 lb) ^
							(Printf.sprintf "%s:\n" la) ^
							(Printf.sprintf "\tli {0},$%d\n" t1) ^
							(Printf.sprintf "%s:\n" lb)
						in
						fcmpopr2s nrd nad nbd (
							match op with
							| Oeq  -> fcmpla (1,2) "feq" (1,0)
							| Oneq -> fcmpla (1,2) "feq" (0,1)
							| Olt  -> fcmpla (1,2) "flt" (1,0)
							| Ogt  -> fcmpla (2,1) "flt" (1,0)
							| Oleq -> fcmpla (2,1) "flt" (0,1)
							| Ogeq -> fcmpla (1,2) "flt" (0,1)
							| _ -> raise (Failure (Printf.sprintf "Operation %s is not float compare operation" os))
						)
					))
				) else (
					biopr2s nrd nad nbd (
						match op with
						| Oadd -> "\tadd {0},{1},{2}\n"
						| Osub -> "\tsub {0},{1},{2}\n"
						| Oeq  -> "\tseq {0},{1},{2}\n"
						| Oneq -> "\tseq {0},{1},{2}\n\tli r6,$1\n\txor {0},{0},r6\n"
						| Olt  -> "\tslt {0},{1},{2}\n"
						| Ogt  -> "\tslt {0},{2},{1}\n"
						| Oleq -> "\tslt {0},{2},{1}\n\tli r6,$1\n\txor {0},{0},r6\n"
						| Ogeq -> "\tslt {0},{1},{2}\n\tli r6,$1\n\txor {0},{0},r6\n"
						| OArrCrt -> (
								let la = genlabel () in
								let lb = genlabel () in
								"\tmov r7,r3\n" ^
								"\tsll r5,{1},$2\n" ^ 
								"\tadd r5,r3,r5\n" ^
								(Printf.sprintf "\tbeq r3,r5,%s\n" lb) ^
								(Printf.sprintf "%s:\n" la) ^ 
								"\tsw {2},r3,$0\n" ^
								"\taddi r3,r3,$4\n" ^
								(Printf.sprintf "\tbne r3,r5,%s\n" la) ^
								(Printf.sprintf "%s:\n" lb) ^ 
								"\tmov {0},r7\n"
					 		)
					 	| OArrRead -> "\tsll r6,{2},$2\n\tadd {0},{1},r6\n\tlw {0},{0},$0\n"
					 	| _ -> raise (Failure (Printf.sprintf "Operation %s from %s is not int binary operation" os (debug_data2str (snd (snd nrd)))))
					)
				)) ^
				"; " ^ (nd2ds nrd) ^ " ::= " ^ os ^ " " ^ (nd2ds nad) ^ " " ^ (nd2ds nbd) ^ "\n"
			)
		| OpOpr(nrd,op,[nad;nbd;ncd]) -> (
				let os = op2str op in
				(triopr2s nrd nad nbd ncd (
					match op with
				 	| OArrWrite -> "\tsll r6,{2},$2\n\tadd r5,{1},r6\n\tsw {3},r5,$0\n"
					| _ -> raise (Failure (Printf.sprintf "Operation %s is not trinary operation" os))
				)) ^
				"; " ^ (nd2ds nrd) ^ " ::= " ^ os ^ " " ^ (nd2ds nad) ^ " " ^ (nd2ds nbd) ^ " " ^ (nd2ds ncd) ^ "\n"
			)
		| OpOpr(_,x,vs) -> raise (Failure (Printf.sprintf "Operation %s with %d argument in not defined yet" (op2str x) (List.length vs)))
	) ops))


let read_all_data filename = 
	let res = ref "" in
	let ic = open_in filename in
	try
		let rec f () = 
			res := !res ^ "\n" ^ (input_line ic);
			f ()
		in f ()
	with
		| End_of_file -> close_in ic; (!res ^ "\n")

let vir2asm (funs,rd,heapvars) = 
	init_heapvars heapvars;
	(read_all_data "lib_tortesia.s") ^
	(String.concat "" (List.map func2asm (List.rev funs))) ^	
	(func2asm {fn=(main_name,(TyVar(-1),default_debug_data)); vs=[]; cvs=[]; body=rd})




