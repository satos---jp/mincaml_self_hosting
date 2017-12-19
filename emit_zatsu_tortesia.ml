open Virtual
open Syntax
open Type_checker
open Debug
open Genint
open Op
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
let on_glob_vars = ref []
let heap_diff = ref 0
let init_globvars gvs = 
	on_glob_vars := List.fold_left (fun r -> fun x -> 
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



let func2asm {fn=(fn,_); vs=vs1; cvs=vs2; body={ops=ops; vs=localvs}} = 
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
	print_string ((String.concat "\n" (List.map (fun (s,p) -> Printf.sprintf "%s :: [r2$%d]" s p) on_stack)) ^"\n");
	print_string ((String.concat "\n" (List.map (fun (s,p) -> Printf.sprintf "%s :: [r4$%d]" s p) on_clos)) ^"\n");
	let na2pt na = (
		match !na with
		| Var x -> (
			try ("r2",List.assoc x on_stack)
			with | Not_found -> 
			try ("r4",List.assoc x on_clos)
			with | Not_found -> 
			raise (Failure "var should be on the stack or closure")
		)
		| GVar x -> (
			try ("r31",List.assoc x !on_glob_vars)
			with | Not_found -> 
			("@" ^ x,-1)
		)
		| Reg _ -> raise (Failure "Register allocation for tortesia is not implemented yet")		
	) in
	let pt2s (a,b) = 
		if String.get a 0 = '@' then String.sub a 1 ((String.length a)-1) else 
			Printf.sprintf "%s,$%d" a b 
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
				(Printf.sprintf "\tlw r5,%s\n\tsw r5,r3,$%d\n" (pt2s (p,l)) (!nl-4))
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
	let mova2b nad nbd = 
			(Printf.sprintf "\tlw r5,%s\n" (nd2ps nbd)) ^
			(Printf.sprintf "\tsw r5,%s\n" (nd2ps nad)) ^
			"; " ^ (nd2ds nad) ^ " ::<= " ^ (nd2ds nbd) ^ "\n"
	in
	let unopr2s nr na s = 
		(Printf.sprintf "\tlw r5,%s\n" (nd2ps na)) ^
		s ^ 
		(Printf.sprintf "\tsw r5,%s\n" (nd2ps nr))
	in
	let biopr2s nr na nb s = 
		(Printf.sprintf "\tlw r6,%s\n" (nd2ps nb)) ^
		(unopr2s nr na s)
	in
	let fbiopr2s nr na nb s = 
		(Printf.sprintf "\tfld f1,%s\n" (nd2ps na)) ^
		(Printf.sprintf "\tfld f2,%s\n" (nd2ps nb)) ^
		s ^ 
		(Printf.sprintf "\tfst f1,%s\n" (nd2ps nr))
	in
	let fcmpopr2s nr na nb s = 
		(Printf.sprintf "\tfld f1,%s\n" (nd2ps na)) ^
		(Printf.sprintf "\tfld f2,%s\n" (nd2ps nb)) ^
		s ^ 
		(Printf.sprintf "\tsw r5,%s\n" (nd2ps nr))
	in
	let triopr2s nr na nb nc s = 
		(Printf.sprintf "\tlw r7,%s\n" (nd2ps nc)) ^
		(biopr2s nr na nb s)
	in
	
	fn ^ ":\n" ^ prologue ^ 
	(String.concat "" (List.map (fun op -> 
		match op with
		| OpMovi((na,(t,d)),CInt(v)) -> assert (t=TyInt); 
			(Printf.sprintf "\tli r5,$%d\n" v) ^ 
			(Printf.sprintf "\tsw r5,%s\n" (na2s na)) ^ 
			"; " ^ (debug_data2simple d) ^ "\n"
		| OpMovi((na,(t,d)),CFloat(v)) -> assert (t=TyFloat); (
				Printf.sprintf "\tfmovi f1,$%.30f\n\tfst f1,%s\n" v (na2s na)
			)
		| OpMov(((na,(t1,d1)) as nrd),((nb,(t2,d2)) as nad)) -> assert (t1=t2); (mova2b nrd nad) 
		| OpLabel x -> x ^ ":\n"
		| OpJcnd(ct,(na,_),(nb,_),la) -> (
				(Printf.sprintf "\tlw r5,%s\n\tlw r6,%s\n" (na2s na) (na2s nb)) ^ 
				(Printf.sprintf "\t%s r5,r6,%s\n" (match ct with 
					| CmpEq -> "bne"
					| CmpLt -> raise (Failure "unimplemented emit CmpLt at virtual.ml")) la)
			)
		| OpJmp(la) -> Printf.sprintf "\tj %s\n" la
		| OpDestTuple(vs,nad) -> (
				let nl = ref 0 in
				(Printf.sprintf "\tlw r5,%s\n" (nd2ps nad)) ^ 
				(String.concat "" (List.map (fun (na,nt) -> 
					let (p,l) = na2pt na in
						nl := !nl + 4;
						(Printf.sprintf "\tlw r6,r5,$%d\n\tsw r6,%s\n" (!nl-4) (pt2s (p,l)))
					) vs)) ^
				"; " ^ (nd2ds nad) ^ "\n"
			)
		| OpMakeTuple(nad,vs) -> (
				(Printf.sprintf "\tsw r3,%s\n" (nd2ps nad)) ^ 
				(make_vs_on_heap vs) ^ 
				"; " ^ (nd2ds nad) ^ "\n"
			)
		| OpMakeCls(nad,((fn,_) as fnd),vs) -> (
				"\tmov r6,r3\n" ^ 
				(make_vs_on_heap vs) ^
				"\tsw r6,r3,$4\n" ^ 
				(Printf.sprintf "\tli r5,%s\n" fn) ^ 
				"\tsw r5,r3,$0\n" ^ 
				(Printf.sprintf "\tsw r3,%s\n" (nd2ps nad)) ^ 
				"\taddi r3,r3,$8\n"^
				"; " ^ (nd2ds nad) ^ " "^ (nd2ds fnd) ^ "\n"
			)
		| OpApp(istail,isdir,nad,((fn,_) as fnd),vs) -> (
				let nl = ref 0 in
				let s = 
				"\tpush r4\n" ^
				(String.concat "" (List.map (fun nad -> 
					nl := !nl + 4;
					(Printf.sprintf "\tlw r5,%s\n" (nd2ps nad)) ^
					"\tpush r5\n"
				) (List.rev vs))) ^ (* 逆にpushする *)
				(let rfn = (na2s fn) in
				if List.mem rfn (global_funcs ()) || isdir = DirApp then 
					(Printf.sprintf "\tjal %s\n" rfn)
				else 
					((Printf.sprintf "\tlw r5,%s\n" rfn) ^ 
					"\tlw r4,r5,$4\n" ^ 
					"\tlw r5,r5,$0\n" ^ 
					"\tjalr r5\n")) ^ 
				(Printf.sprintf "\tsw r5,%s\n" (nd2ps nad))
				in (* こうしないと、nlがアップデートされない *)
				s ^ 
				(Printf.sprintf "\taddi r1,r1,$%d\n" !nl) ^
				"\tpop r4\n" ^
				"; " ^ (nd2ds nad) ^ " " ^ (nd2ds fnd) ^ "\n"
			)
		| OpRet((na,nt)) -> (
				(let (p,l) = na2pt na in
					(Printf.sprintf "\tlw r5,%s\n" (pt2s (p,l)))
					) ^
				epilogue
			)
		| OpMainRet -> (
				"\tli r5,$0\n" ^
				epilogue
			)
		| OpOpr(nrd,Osemi2,[_;nbd]) -> mova2b nrd nbd
		| OpOpr(nrd,Osemi1,[nad]) -> mova2b nrd nad
		| OpOpr(nrd,op,[nad]) -> (
				let rec tomul a = 
					(if a <= 0 then "" else (tomul (a/2))) ^
						"\tsll r6,r6,$1\n" ^
						(if (a mod 2 <> 0) then "\tadd r6,r6,r5\n" else "")
				in
				unopr2s nrd nad (
					match op with
					| Ominus -> "\tsub r5,r0,r5\n"
					| Onot -> "\tslti r6,r5,$0\n\tslti r7,r5,$1\n\tsub r5,r7,r6\n"
					| OGetTuple(i) -> (Printf.sprintf "\tlw r5,r5,$%d\n" (i*4))
					| Oimul(x) -> (
							"\tli r6,$0\n" ^ (tomul x) ^ "\tmov r5,r6\n"
						)
					| Oibydiv(x) -> (
						match x with
						| 2 -> "\tslti r6,r5,$0\n\tadd r5,r5,r6\n\tsra r5,r5,$1\n"
						(* from https://stackoverflow.com/questions/5558492/divide-by-10-using-bit-shifts *)
						| 10 -> (
								"\tli r6,$0\n" ^ (tomul 205) ^ "\tsra r5,r6,$11\n"
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
							| Ofadd -> "\tfadd f1,f1,f2\n"
							| Ofsub -> "\tfsub f1,f1,f2\n"
							| Ofmul -> "\tfmul f1,f1,f2\n"
							| Ofdiv -> "\tfdiv f1,f1,f2\n"
							| _ -> raise (Failure (Printf.sprintf "Operation %s is not float binary operation" os))
						)
					) else (
						let fcmpla (f1,f2) cs (t1,t2) = 
							let la = genlabel () in
							let lb = genlabel () in
							(Printf.sprintf "\t%s f%d,f%d\n" cs f1 f2) ^ 
							(Printf.sprintf "\tbft %s\n" la) ^
							(Printf.sprintf "\tli r5,$%d\n\tj %s\n" t2 lb) ^
							(Printf.sprintf "%s:\n" la) ^
							(Printf.sprintf "\tli r5,$%d\n" t1) ^
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
						| Oadd -> "\tadd r5,r5,r6\n"
						| Osub -> "\tsub r5,r5,r6\n"
						| Oeq  -> "\tseq r5,r5,r6\n"
						| Oneq -> "\tseq r5,r5,r6\n\tli r6,$1\n\txor r5,r5,r6\n"
						| Olt  -> "\tslt r5,r5,r6\n"
						| Ogt  -> "\tslt r5,r6,r5\n"
						| Oleq -> "\tslt r5,r6,r5\n\tli r6,$1\n\txor r5,r5,r6\n"
						| Ogeq -> "\tslt r5,r5,r6\n\tli r6,$1\n\txor r5,r5,r6\n"
						| OArrCrt -> (
								let la = genlabel () in
								let lb = genlabel () in
								"\tmov r7,r3\n" ^
								"\tsll r5,r5,$2\n" ^ 
								"\tadd r5,r3,r5\n" ^
								(Printf.sprintf "\tbeq r3,r5,%s\n" lb) ^
								(Printf.sprintf "%s:\n" la) ^ 
								"\tsw r6,r3,$0\n" ^
								"\taddi r3,r3,$4\n" ^
								(Printf.sprintf "\tbne r3,r5,%s\n" la) ^
								(Printf.sprintf "%s:\n" lb) ^ 
								"\tmov r5,r7\n"
					 		)
					 	| OArrRead -> "\tsll r6,r6,$2\n\tadd r5,r5,r6\n\tlw r5,r5,$0\n"
					 	| _ -> raise (Failure (Printf.sprintf "Operation %s from %s is not int binary operation" os (debug_data2str (snd (snd nrd)))))
					)
				)) ^
				"; " ^ (nd2ds nrd) ^ " ::= " ^ os ^ " " ^ (nd2ds nad) ^ " " ^ (nd2ds nbd) ^ "\n"
			)
		| OpOpr(nrd,op,[nad;nbd;ncd]) -> (
				let os = op2str op in
				(triopr2s nrd nad nbd ncd (
					match op with
				 	| OArrWrite -> "\tsll r6,r6,$2\n\tadd r5,r5,r6\n\tsw r7,r5,$0\n"
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

let vir2asm (funs,rd,globvars) = 
	init_globvars globvars;
	(read_all_data "lib_zatsu_tortesia.s") ^
	(String.concat "" (List.map func2asm (List.rev funs))) ^	
	(func2asm {fn=(main_name,(TyVar(-1),default_debug_data)); vs=[]; cvs=[]; body=rd})




