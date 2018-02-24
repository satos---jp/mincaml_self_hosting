open Virtual
open Syntax
open Type_checker
open Debug
open Genint
open Op
open Cfg
open Str
open Main_option
open Tortesia_register_convention

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
			let nl = 4 in
			match !na with
			| Var tna -> f ((tna,sl) :: ar,nl+sl) xs
			| Reg _ | GVar _ -> f (ar,nl+sl) xs
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


let mapi f vs = 
	let rec g i =
		function
		| [] -> []
		| x :: xs -> (f i x) :: (g (i+1) xs)
	in g 0 vs

let func2asm {fn=(fn,_); vs=vs; regs=regs; cvs=cvs; body={ops=ops; vs=localvs}} = 
	(*
	Printf.printf "%s%s%s" (Closure_conv.vs2str vs1) (Closure_conv.vs2str vs2) (Closure_conv.vs2str localvs);
	*)
	let lvs_st = vs2stacks localvs in
	let vs_st = vs2stacks vs in
	let cvs_st = vs2stacks cvs in
	
	let on_stack = 
		(List.map (fun (x,p) -> (x,p-(snd lvs_st))) (fst lvs_st)) @
		(List.map (fun (x,p) -> (x,p+8)) (fst vs_st)) in
	
	let on_clos = fst cvs_st in
	print_string ("On function " ^ fn ^ "\n");
	print_string ((String.concat "\n" (List.map (fun (s,p) -> Printf.sprintf "%s :: [r2$%d]" s p) on_stack)) ^"\n");
	print_string ((String.concat "\n" (List.map (fun (s,p) -> Printf.sprintf "%s :: [r4$%d]" s p) on_clos)) ^"\n");

	let na2s x = 
		try Printf.sprintf "r2,$%d" (List.assoc x on_stack)
		with | Not_found -> 
		try Printf.sprintf "r4,$%d" (List.assoc x on_clos)
		with | Not_found -> 
		try Printf.sprintf "r31,$%d" (List.assoc x !on_heap_vars) (* 正直ガバなのでどうにかしたい *)
		with | Not_found -> 
		x
	in
	let ls2r ls r (na,_) = (
		let res,rg = 
		match !na with
		| Var x | GVar x -> (Printf.sprintf "\t%s %s,%s\n" ls r (na2s x)), r
		| Reg x -> "",x
		in
		res,rg
	) in
	let nd2ds (_,(_,d)) = (debug_data2simple d) in
	let mova2b ((nr,(t,_)) as nrd) ((na,_) as nad) = 
		if !nr = !na then "" else  
		((if t = TyFloat then (
			match !nr,!na with
			| Var r,Var a | Var r,GVar a | GVar r,Var a | GVar r,GVar a -> (fst (ls2r "fld" "f5" nad)) ^ (fst (ls2r "fst" "f5" nrd))
			| Reg r,Var a | Reg r,GVar a -> Printf.sprintf "\tfld %s,%s\n" r (na2s a)
			| Var r,Reg a | GVar r,Reg a -> Printf.sprintf "\tfst %s,%s\n" a (na2s r)
			| Reg r,Reg a -> Printf.sprintf "\tfmov %s,%s\n" r a
		) else (
			match !nr,!na with
			| Var r,Var a | Var r,GVar a | GVar r,Var a | GVar r,GVar a -> (fst (ls2r "lw" "r5" nad)) ^ (fst (ls2r "sw" "r5" nrd))
			| Reg r,Var a | Reg r,GVar a -> Printf.sprintf "\tlw %s,%s\n" r (na2s a)
			| Var r,Reg a | GVar r,Reg a -> Printf.sprintf "\tsw %s,%s\n" a (na2s r)
			| Reg r,Reg a -> Printf.sprintf "\tmov %s,%s\n" r a
		))) ^		"; " ^ (nd2ds nad) ^ " ::<= " ^ (nd2ds nrd) ^ "\n"
	in
	let movs tr r t = 
		if r = tr then "" else 
		match t with
		| TyFloat -> (Printf.sprintf "\tfmov %s,%s\n" tr r)
		| _ -> (Printf.sprintf "\tmov %s,%s\n" tr r)
	in
	let v2reg tr ((_,(t,_)) as na) = 
		let s,r = ls2r "lw" tr na in
		if tr = r then s else movs tr r t
	in
	let reg2v ((_,(t,_)) as na) fr = 
		let s,r = ls2r "sw" fr na in
		if fr = r then s else movs r fr t	
	in
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
	(* callee save することにする *)
	(* r5は、返り値なので抜いておく *)
	
	let prologue = 
		(if fn = main_name then Printf.sprintf "\tmov r31,r3\n\taddi r3,r3,$%d\n" !heap_diff else "") ^
		
		"\tmflr r6\n" ^ 
		(Printf.sprintf "\tsubi r1,r1,$4\n\tsw r2,r1,$0\n\tmov r2,r1\n\tsubi r1,r1,$%d\n" (snd lvs_st)) ^
		
		(* レジスタ退避 *)
		((Printf.sprintf "\tsubi r1,r1,$%d\n") ((List.length regs) * 4 + 4)) ^
		(String.concat "" (List.mapi (fun i r -> Printf.sprintf "\tsw %s,r1,$%d\n" r (i*4)) ("r6" :: regs))) ^
		
		(ivprint (Printf.sprintf "vs length %d\n" (List.length vs));
		(* 引数を、必要ならば指定のレジスタに割り当てておく *)
		let f r ((x,(t,_)) as na) = 
			match !x,t with
			| Reg tr,TyFloat -> Printf.sprintf "\tfmov %s,%s\n" tr r
			| Reg tr,_ -> Printf.sprintf "\tmov %s,%s\n" tr r
			| _ -> (
				ivprint (Printf.sprintf "arg %s is not register" (namestr2str !x));
				reg2v na r
			)
		in
		(String.concat "" (tortesia_register_convention.args2regs vs
			f
			f
			(fun i (x,_) -> 
				match !x with
				| Reg tr ->  Printf.sprintf "\tlw %s,r2,$%d\n" tr (8+i*4) (* loadする必要あり *)
				| _ -> ""
			)
		))) ^
		(* クロージャ引数を、必要ならば指定のレジスタに割り当てておく *)
		(String.concat "" (mapi (fun i (x,_) -> 
			match !x with
			| Reg r -> (
				Printf.sprintf "\tlw %s,r4,$%d\n" r (i*4)
			)
			| _ -> ""
		) cvs))
	in
	ivprint (Printf.sprintf "Plorogue:\n %s" prologue);
	let epilogue = (
		(if fn = main_name then "\thlt\n" else 
		(* レジスタ復帰 *)
		(String.concat "" (List.mapi (fun i r -> Printf.sprintf "\tlw %s,r1,$%d\n" r (i*4)) ("r6" :: regs))) ^
		((Printf.sprintf "\taddi r1,r1,$%d\n") ((List.length regs) * 4 + 4)) ^
		
		((Printf.sprintf "\taddi r1,r1,$%d\n\tlw r2,r1,$0\n\taddi r1,r1,$4\n" (snd lvs_st)) ^	
		"\tjr r6\n"))
	)
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
		| OpJcnd(ct,((_,(t,_)) as na),nb,la) -> (
					let s1,r1 = ls2r "lw" "r5" na in
					let s2,r2 = ls2r "lw" "r6" nb in
					s1 ^ s2 ^ 
					(match ct with
					| CmpEq -> Printf.sprintf "\tbne %s,%s,%s\n" r1 r2 la
					| CmpLt -> (
						match t with
						| TyFloat -> Printf.sprintf "\tflt %s,%s\n\tbft %s\n" r1 r2 la
						| _ -> Printf.sprintf "\tslt r5,%s,%s\n\tbne r0,r5,%s\n" r1 r2 la
					)
					)
			)
		| OpJmp(la) -> Printf.sprintf "\tj %s\n" la
		| OpDestTuple(vs,na) -> (
				let nl = ref (-4) in
				let s1,r1 = ls2r "lw" "r5" na in
				(* このr1を使うと破壊してしまう可能性がある *)
				let s1,r1 = (if r1 <> "r5" then s1 ^ (Printf.sprintf "\tmov r5,%s\n" r1) else s1),"r5" in
				s1 ^ 
				(String.concat "" (List.map (fun nb -> 
					let s2,r2 = ls2r "sw" "r6" nb in
					nl := !nl + 4;
					(Printf.sprintf "\tlw %s,%s,$%d\n" r2 r1 !nl) ^
					s2 
				) vs)) ^
				"; " ^ (nd2ds na) ^ "\n"
			)
		| OpMakeTuple(nr,vs) -> (
				(* 長さ0のタプルについては、何もしなくてよい。 *)
				(if vs = [] then "" else (
					"\tmov r6,r3\n" ^ 
					(make_vs_on_heap vs) ^ 
					(reg2v nr "r6") (* 伏線回収 *)
				)) ^ 
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
		| OpApp(istail,isdir,((_,(t,_)) as nr),((fn,_) as fnd),vs) -> (
				let nl = 4 * (List.length vs) in (* これ、もう割り当てておくことにする *)
				let s = 
				"\tsubi r1,r1,$4\n\tsw r4,r1,$0\n" ^
				(* とりあえず引いておく。(これがないと、子で割り当てられないときにつむ) *)
				(Printf.sprintf "\tsubi r1,r1,$%d\n" nl) ^
				(* 引数をレジスタにする *)
				(String.concat "" (tortesia_register_convention.args2regs vs 
					(fun r x -> mova2b (ref (Reg r),snd x) x)
					(fun f x -> mova2b (ref (Reg f),snd x) x)
					(fun i ((na,(t,_)) as x) -> 
						let r = (match !na with Reg tr -> tr | _ -> tortesia_register_convention.ty2retreg t) in
						let s,r = ls2r "lw" r x in
						s ^ 
						(Printf.sprintf "\tsw %s,r1,$%d\n" r (i*4))
					)
				)) ^ 
				
				(* call *)
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
				(reg2v nr (tortesia_register_convention.ty2retreg t))
				in (* こうしないと、nlがアップデートされない *)
				s ^ 
				(Printf.sprintf "\taddi r1,r1,$%d\n" nl) ^
				"\tlw r4,r1,$0\n\taddi r1,r1,$4\n" ^
				"; " ^ (nd2ds nr) ^ " " ^ (nd2ds fnd) ^ "\n"
			)
		| OpRet((_,(t,_)) as na) -> (
				(v2reg (tortesia_register_convention.ty2retreg t) na) ^
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
					| Oiadd(i) -> Printf.sprintf "\taddi {0},{1},$%d\n" i
					| Oibysub(i) -> Printf.sprintf "\tsubi {0},{1},$%d\n" i
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
					| OiArrRead(a) -> Printf.sprintf "\tlw {0},{1},$%d\n" (a*4)
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
					 	| OArrRead -> "\tsll r6,{2},$2\n\tadd r5,{1},r6\n\tlw {0},r5,$0\n"
					 	| OiArrWrite(a) -> Printf.sprintf "\tsw {2},{1},$%d\n" (a*4)
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
	(read_all_data "lib/lib_tortesia.s") ^
	(if !in_out_assembler then (read_all_data "lib/lib_tortesia_in_out.s") else "") ^
	(String.concat "" (List.map func2asm (List.rev funs))) ^	
	(func2asm {fn=(main_name,(TyVar(-1),default_debug_data)); vs=[]; regs=[]; cvs=[]; body=rd})




