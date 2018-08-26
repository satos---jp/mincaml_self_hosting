open Syntax
open Debug
open Main_option
open Genint

type tyvar = int

type type_name = string

type ty =
	| TyInt
	| TyFloat
	| TyNum  (* int も float もこれの部分型 *)
	| TyVar of tyvar
	| TyArr of ty
	| TyFun of (ty list) * ty
	| TyTuple of ty list
	| TyUserDef of type_name


exception TypeError of ty * ty * Debug.debug_data

let tyvar2str v = Printf.sprintf "'a%d" v

let rec type2str_with_pa t =
        match t with
        | TyInt | TyFloat | TyVar _ | TyTuple _ | TyNum | TyUserDef _ -> type2str t
        | TyFun _ | TyArr _ -> "(" ^ (type2str t) ^ ")"

and type2str t =
	match t with
	| TyInt -> "int"
	| TyFloat -> "float"
	| TyNum -> "number"
	| TyVar v -> tyvar2str v
	| TyFun (t1, t2) -> (
		Printf.sprintf "%s -> %s" 
			(String.concat " -> " (List.map type2str_with_pa t1))
			(type2str_with_pa t2)
		)
	| TyArr t -> Printf.sprintf "Array %s" (type2str_with_pa t)
	| TyTuple ts -> (
			match ts with
			| [] -> "()"
			| x :: xs -> 
				"(" ^ (List.fold_left (fun r -> fun t -> r ^ " * " ^ (type2str_with_pa t)) (type2str_with_pa x) xs) ^ ")"
		)
	| TyUserDef(s) -> "UsrDef(" ^ s ^ ")"

let print_type t = print_string ((type2str t) ^ "\n")

let gentype () = TyVar(genint ())

let genvar () = Printf.sprintf "@tc_%d" (genint ())

let name2str (na,ty) = na ^ " : " ^ (type2str ty)

type name = string * (ty * debug_data)

type texp = texp_base * (ty * debug_data)
and texp_base =
  | TConst of const
  | TVar       of name
  | TOp        of optype * (texp list)
  | TIf        of texp * texp * texp
  | TLet       of name * texp * texp
  | TLetRec    of name * (name list) * texp * texp
  | TApp       of texp * (texp list)
  | TTuple     of (texp list)
  | TLetTuple  of (name list) * texp * texp

let tdname2str (na,(ty,d)) = na ^ " : " ^ (type2str ty) ^ " : " ^ (debug_data2str d)

let rec texp2str_base astdeb d = 
	let ast,_ = astdeb in
	match ast with
	| TConst x -> [(d,const2str x)]
	| TVar(x) -> [(d,"Var " ^ (tdname2str x))]
	| TOp(op,es) -> (d,op2str op) :: List.concat (List.map (fun x -> (texp2str_base x (d+1))) es)
	| TIf(e1,e2,e3) -> (
			(d,"If") :: (texp2str_base e1 (d+1))
				@ [(d,"Then")] @ (texp2str_base e2 (d+1)) @ [(d,"Else")] @ (texp2str_base e3 (d+1))
		)
	| TLet(na,e1,e2) -> (d,"Let " ^ (tdname2str na) ^ " =") :: (texp2str_base e1 (d+1)) @ [(d,"In")] @ (texp2str_base e2 (d+1))
	| TLetRec(na,vs,e1,e2) -> (d,"LetRec " ^ (tdname2str na) ^ " =") :: (texp2str_base e1 (d+1)) @ [(d,"In")] @ (texp2str_base e2 (d+1))
	| TApp(e1,es) -> (d,"App ") :: (texp2str_base e1 (d+1)) @ List.concat (List.map (fun x -> (texp2str_base x (d+2))) es)
	| TTuple(es) -> (d,"( ") :: List.concat (List.map (fun x -> (texp2str_base x (d+1))) es) @ [(d," )")]
	| TLetTuple(vs,e1,e2) -> (d,"Let " ^ (String.concat " , " (List.map tdname2str vs)) ^ " = ") :: (texp2str_base e1 (d+1)) @ [(d,"In")] @ (texp2str_base e2 (d+1))

let texp2str ast = 
	let ss = texp2str_base ast 0 in
		String.concat "\n" (List.map (fun (d,s) -> (String.make (d*2) ' ') ^ s) ss) ^ "\n"


let genv () = 
if !tortesia then ((if !asmsin_asmint then [
	("print_int",TyFun([TyInt],TyTuple([])));

	("sin",TyFun([TyFloat],TyFloat));
	("cos",TyFun([TyFloat],TyFloat));
	("atan",TyFun([TyFloat],TyFloat));
] else []) @ [
	(* ハードウェア実装してもらう *)
	("print_char",TyFun([TyInt],TyTuple([])));
	
	("read_char",TyFun([TyTuple([])],TyInt));
	("read_int",TyFun([TyTuple([])],TyInt));
	("read_float",TyFun([TyTuple([])],TyFloat));
(*
ハードウェアでやってもらうのは、
入力 は print_int　と print_float
出力 は print_char のみ、で、
print_int はコンパイラで実装する。
*)
(* アセンブラで実装した *)
	("float_of_int",TyFun([TyInt],TyFloat));
	("int_of_float",TyFun([TyFloat],TyInt));
	("sqrt",TyFun([TyFloat],TyFloat));   (* x -> root x のほう *)
(* とりあえず、x86のものを流用する *)
]) else [ (*x86 *)
(* アセンブラで実装した*)
	("fless",TyFun([TyFloat;TyFloat],TyInt));
	("int_of_float",TyFun([TyFloat],TyInt));
	("float_of_int",TyFun([TyInt],TyFloat));
	("print_char",TyFun([TyInt],TyTuple([])));
	("print_char_err",TyFun([TyInt],TyTuple([])));
	("read_char",TyFun([TyTuple([])],TyInt));
	("fiszero",TyFun([TyFloat],TyInt));
	("fispos",TyFun([TyFloat],TyInt));
	("fisneg",TyFun([TyFloat],TyInt));
	("fabs",TyFun([TyFloat],TyFloat));
	("floor",TyFun([TyFloat],TyFloat));
	("fsqr",TyFun([TyFloat],TyFloat)); (* x -> x^2 のほう *)
	("fneg",TyFun([TyFloat],TyFloat));
	("fhalf",TyFun([TyFloat],TyFloat));
	("sqrt",TyFun([TyFloat],TyFloat));   (* x -> root x のほう *)
	("sin",TyFun([TyFloat],TyFloat));
	("cos",TyFun([TyFloat],TyFloat));
	("atan",TyFun([TyFloat],TyFloat));
(* それぞれ、専用の入力命令があるため、アセンブラにした *)
	("read_int",TyFun([TyTuple([])],TyInt));
	("read_float",TyFun([TyTuple([])],TyFloat));

(* れいず!! *)
	("raise_match_failure",TyFun([TyTuple([])],TyTuple([])));
]

let global_funcs () = List.map fst (genv ())

let zip2 = List.map2 (fun a -> fun b -> (a,b))
let zip3 vs ws = List.map2 (fun (a,b) -> fun c -> (a,b,c)) (zip2 vs ws)



(*
# type a = H of (int * int * int);;
type a = H of (int * int * int)
# fun x -> match x with H p -> p;;
- : a -> int * int * int = <fun>

# type a = H of int * int * int;;
type a = H of int * int * int
# fun x -> match x with H p -> p;;
Error: The constructor H expects 3 argument(s),
       but is applied here to 1 argument(s)

まーた闇なんだが
*)

(*
# type b = Y of unit;; 
# fun x -> match x with Y() -> 3;;    (この () は value-name)
- : b -> int = <fun>
# fun x -> match x with Y(_) -> 3;;   (この () は  ( pattern )  のやつ)
- : b -> int = <fun>
# fun x -> match x with Y(_,_) -> 3;;
Error: This pattern matches values of type 'a * 'b
       but a pattern was expected which matches values of type unit

:thinking_face:
*)


(*
	型環境        env
	型定義環境    venv : (string * (int * ty * (ty list))) list
	パターン pat
	pat に対応する変数名 pna
*)

(* cond : texp, 
	 bind : texp -> texp,
	 tenv : (string * ty) list,
	 constr : (ty * ty * debug) list
を返す *)
(* マッチをif文の連鎖にするので、cond に成功したら (bind e),みたいなかんじで。 *)
let rec pattern2env env venv pat pna = 
	let f = pattern2env env venv in
	let _,td = pna in
	let pt,deb = td in
	let true_expr = TConst(CInt(1)),(TyInt,deb) in
	let false_expr = TConst(CInt(0)),(TyInt,deb) in
	let cmptag tagn = (
		TOp(Oeq,[
			(TConst(CInt(tagn)),(TyInt,deb));
			(TOp(OGetTuple(0),[pna]),(TyInt,deb))
		]),(TyInt,deb)
	) in
	match pat with
	| PVar(x) -> (
			true_expr,
			(fun ((_,ted) as e) -> TLet((x,td),pna,e),ted),
			[(x,pt)],
			[]
		)
	| PVariant(tag) -> (
			let tagn,tagty,tagarg = (
				try
					List.assoc tag venv
				with
					| Not_found -> raise (Failure("Undefined variant tag " ^ tag ^ " at " ^ (debug_data2str deb)))
			) in
			let ls = List.length tagarg in
			if ls <> 0 then
				raise (Failure(Printf.sprintf "tag %s needs %d args at %s" tag ls (debug_data2str deb)))
			else (
				(cmptag tagn),
				(fun e -> e),
				[],
				[(pt,tagty,deb)]
			)
		)
	| PVariantApp(tag,ps) -> (
			let tagn,tagty,tagarg = (
				try
					List.assoc tag venv 
				with
					| Not_found -> raise (Failure("Undefined variant tag " ^ tag ^ " at " ^ (debug_data2str deb)))
			) in
			let ls = List.length tagarg in
			Printf.printf "%s :: %d\n" tag ls; 
			if ls == 1 then (
				let nt = List.hd tagarg in
				let na = (genvar (),(nt,deb)) in
				let cond, bind, tenv, cs = f ps (TVar(na),(nt,deb)) in
				(
					(TIf(cmptag tagn,cond,false_expr),(TyInt,deb)),
					(fun ((_,ted) as e) -> TLet(na,
						(TOp(OGetTuple(0),[
							TOp(OGetTuple(1),[pna]),(nt,deb)
						]),(TyTuple([nt]),deb))
					,(bind e)),ted),
					tenv,
					(pt,tagty,deb) :: cs
				)
			) else (
				match ps with
				| PTuple rps when (List.length rps == ls) -> (
						let nt = TyTuple(tagarg) in
						let na = (genvar (),(nt,deb)) in
						let cond, bind, tenv, cs = f ps (TVar(na),(nt,deb)) in
						(
							(TIf(cmptag tagn,cond,false_expr),(TyInt,deb)),
							(fun ((_,ted) as e) -> TLet(na,
								(TOp(OGetTuple(1),[pna]),(nt,deb))
							,(bind e)),ted),
							tenv,
							(pt,tagty,deb) :: cs
						)
					)
				| _ -> raise (Failure(Printf.sprintf "tag %s needs %d args at %s" tag ls (debug_data2str deb)))
			)			
		)
	| PTuple(ps) -> (
			let ((cond, bind, rtenv, cs),ts) = 
				List.fold_left (fun ((rcond, rbind, rtenv, rcs),ts) (i,pat) -> 
					let nt = gentype () in
					let na = (genvar (),(nt,deb)) in
					let cond, bind, tenv, cs = f pat (TVar(na),(nt,deb)) in
					((
						(TIf(rcond,cond,false_expr),(TyInt,deb)),
						(fun ((_,ted) as e) -> TLet(na,
							(TOp(OGetTuple(i),[pna]),(nt,deb))
						,(bind (rbind e))),ted),
						tenv @ rtenv,
						cs @ rcs
					),nt :: ts)
				) ((true_expr,(fun x -> x),env,[]),[]) (List.mapi (fun i x -> (i,x)) ps)
			in
				(cond, bind, rtenv, (TyTuple(List.rev ts),pt,deb) :: cs)
		)


(* ここで、型情報をastに載せる必要がある *)

(* astdebは、 ast, deb の組。 *)
(* 環境は、 (変数名,(型とデバッグ情報)) *)
(* 返り値は、 (新ast,制約,型,debug情報) *)
(* 制約は、型1,型2,デバッグ情報 *)
(* ここでは、まだα変換はしないことにしました *)

(* TyInt関連は最後にチェックする *)
let addglobalcs = ref []

let rec type_infer venv astdeb env = 
	let f = type_infer venv in
	let ast,deb = astdeb in
	let rast,rc,rt = (
	match ast with
	| EConst(CFloat x) -> (TConst(CFloat x),[],TyFloat)
	| EConst(CInt x) -> (TConst(CInt x),[],TyInt)
	| EVar(x) -> (
			try (
				let tt = List.assoc x env in
				(TVar((x,(tt,deb))),[],tt)
			)
			with
				| Not_found -> raise (Failure("undefined variable " ^ x ^ " at " ^ (debug_data2str deb)))
		)
	| EOp(op,vs) -> (
			let tects = List.map (fun x -> f x env) vs in
			let tes = List.map (fun (x,_,_,_) -> x) tects in
			let tcs = List.concat (List.map (fun (_,x,_,_) -> x) tects) in
			let tts = List.map (fun (_,_,x,_) -> x) tects in 
			let tds = List.map (fun (_,_,_,x) -> x) tects in 
			
			let tyf = (match op with
			| Onot | Ominus | Oiadd _ | Oibysub _ | Oimul _ | Oibydiv _ -> (fun () -> ([TyInt],TyInt))
			| Oadd | Osub | Omul | Odiv -> (fun () -> ([TyInt;TyInt],TyInt))
			| Ofadd | Ofsub | Ofmul | Ofdiv -> (fun () -> ([TyFloat;TyFloat],TyFloat))
			
			(* 全て、float同士、でもいけるようにする(多相性) *)
			| Oeq | Oneq | Olt | Oleq | Ogt | Ogeq -> (
				(fun () -> let x = gentype () in  
					addglobalcs := [(x,TyNum,deb)] @ !addglobalcs;
					([x;x],TyInt))
			)
			
			| Osemi2 -> (fun () -> let x = gentype () in ([TyTuple([]);x],x))
			| Osemi1 -> (fun () -> let x = gentype () in ([x],x))
			| OArrCrt -> (fun () -> let x = gentype () in ([TyInt;x],TyArr(x)))
			| OArrRead -> (fun () -> let x = gentype () in ([TyArr(x);TyInt],x))
			| OArrWrite -> (fun () -> let x = gentype () in ([TyArr(x);TyInt;x],TyTuple([])))
			| OiArrRead _ -> (fun () -> let x = gentype () in ([TyArr(x)],x))
			| OiArrWrite _ -> (fun () -> let x = gentype () in ([TyArr(x);x],TyTuple([])))
			
			| OSubTuple _ | OGetTuple _ -> raise (Failure (Printf.sprintf "%s shouldn't appear in parsed syntax" (op2str op)))
			) in
			let nts,rt = tyf () in
				(TOp(op,tes),(zip3 nts tts tds) @ tcs,rt)
		)
	| EIf(e1,e2,e3) -> (
			let te1,c1,tt1,deb1 = f e1 env in
			let te2,c2,tt2,_ = f e2 env in
			let te3,c3,tt3,deb3 = f e3 env in
				(TIf(te1,te2,te3),(tt1,TyInt,deb1) :: (tt2,tt3,deb3) :: c1 @ c2 @ c3,tt2)
		)
	| ELet(n1,e2,e3) -> (
			let te2,c2,tt2,deb2 = f e2 env in
			let te3,c3,tt3,_ = f e3 ((n1,tt2) :: env) in
			(TLet((n1,(tt2,deb2)),te2,te3),c2 @ c3,tt3)
		)
	| ELetRec(f1,ns,e2,e3) -> (
			(* fの返り値型 *)
			let fn1rt = gentype () in
			(* nsのα変換と、 型変数を作ったもの *)
			let tns = List.map (fun x -> (x,gentype ())) ns in
			(* fの型 *)
			let f1t = TyFun(List.map (fun (_,x) -> x) tns,fn1rt) in
			(* e2についての推論 *) 
			let te2,c2,tt2,deb2 = f e2 (tns @ [(f1,f1t)] @ env) in
			(* これは、関数名より変数名の方が先に調べられるみたい *) 
			(* e3についての推論 *)
			let te3,c3,tt3,_ = f e3 ((f1,f1t) :: env) in
			(TLetRec((f1,(f1t,deb2)),List.map (fun (x,t) -> (x,(t,deb2))) tns,te2,te3),(tt2,fn1rt,deb2) :: c2 @ c3,tt3)
		)
	| EApp(e1,e2) -> (
		(* カリー化できないのでがんばる *)
			let te1,c1,tt1,deb1 = f e1 env in
			let rt = gentype () in
			let tet = List.map (fun x -> f x env) e2 in
			let funt = TyFun(List.map (fun (_,_,x,_) -> x) tet,rt) in
				(TApp(te1,List.map (fun (x,_,_,_) -> x) tet),
				 (tt1,funt,deb1) :: List.concat (List.map (fun (_,x,_,_) -> x) tet),
				 rt)
		)
	| ETuple(et) -> (
		(* 制約集合の和をとるだけにする。 *)
			let tet = List.map (fun x -> f x env) et in
				(TTuple(List.map (fun (x,_,_,_) -> x) tet),
				 List.concat (List.map (fun (_,x,_,_) -> x) tet),
				 TyTuple(List.map (fun (_,_,x,_) -> x) tet))
		)
	| ELetTuple(ns,e1,e2) -> (
			let te1,c1,tt1,deb1 = f e1 env in
			let tns = List.map (fun x -> (x,gentype ())) ns in
			let te2,c2,tt2,_ = f e2 (tns @ env) in
				(TLetTuple(List.map (fun (x,t) -> (x,(t,deb1))) tns,te1,te2),
				(tt1,TyTuple(List.map (fun (_,x) -> x) tns),deb1) :: c1 @ c2,tt2)
		)
	| EVariant(tag,es) -> (
			let tagn,tagty,args = (
				try
					List.assoc tag venv
				with
					| Not_found -> raise (Failure("Undefined variant tag " ^ tag ^ " at " ^ (debug_data2str deb)))
			) in
			let tagne = TConst(CInt tagn),(TyInt,deb) in
			let tet = List.map (fun x -> f x env) es in
			let tt = TyTuple(List.map (fun (_,_,x,_) -> x) tet) in
			let tagcs = (
				try 
					List.map2 (fun t (_,_,x,_) -> (t,x,deb)) args tet 
				with
				| Invalid_argument("List.map2") -> raise (Failure(
						Printf.sprintf "tag %s expects %d args but given %d args at %s" tag (List.length args) (List.length tet) (debug_data2str deb)
					))
			) in
			(TTuple([tagne;(TTuple(List.map (fun (x,_,_,_) -> x) tet),(tt,deb))]),
			 tagcs @ List.concat (List.map (fun (_,x,_,_) -> x) tet),
			 tagty)
		)
	| EMatch(ep,ps) -> (
			(* if文に訳する *)
			let te1,c1,tt1,deb1 = f ep env in
			let na = genvar (),(tt1,deb1) in
			let nav = TVar(na),(tt1,deb1) in
			(* ニュアンスは関数とその適用がめっちゃある感じ *)
			let tet =  List.map (fun (pat,e) ->  
				let cond,bind,aenv,cs = pattern2env env venv pat nav in
				let tenv = aenv @ env in
				(cond,bind,cs),(f e tenv)
			) ps in  
			
			let rt = gentype () in
			let ert = TyFun([gentype ()],rt) in
			let erre : texp = (TApp(
				(TVar(("raise_match_failure",(ert,deb))),(ert,deb)),
				[TConst(CInt(0)),(TyInt,deb)]
			),(rt,deb)) in
			let re = TLet(na,te1,(
				List.fold_right (fun ((cond,bind,_),(x,_,_,d)) ne -> 
					TIf(cond,bind x,ne),(rt,deb)
				) tet erre
			)) in 
			let rcs = (
				(List.map (fun (_,(_,_,t,_)) -> (t,rt,deb)) tet) @ 
				(List.concat (List.map (fun ((_,_,c1),(_,c2,_,_)) -> (c1 @ c2)) tet)) @ 
				c1
			) in
				(re,rcs,rt)
		)
	) in
	((rast,(rt,deb)),rc,rt,deb)



let rec print_constrs_sub cs =
        match cs with
        | [] -> ()
        | (x,y,_) :: xs -> (
        	Printf.printf "(%s , %s)\n" 
                (type2str x) (type2str y);
                print_constrs_sub xs)

let print_constrs cs =
        print_string "[\n";
        print_constrs_sub cs;
        print_string "]\n"

let print_subs subs =
	print_string "[\n";
	List.iter (fun (a,b) -> 
		Printf.printf "%s => %s\n" (tyvar2str a) (type2str b);
	) subs;
	print_string "]\n"


let rec ty_var_appear t v =
        match t with
        | TyInt | TyFloat | TyNum | TyUserDef _ -> false
        | TyFun (t1s, t2) -> List.exists (fun x -> ty_var_appear x v) (t2 :: t1s)
        | TyVar x -> x = v
        | TyArr t -> (ty_var_appear t v)
        | TyTuple ts -> List.fold_left (fun r -> fun t -> r || (ty_var_appear t v)) false ts

let rec ty_subst subs t = 
	match t with
	| TyInt | TyFloat | TyNum | TyUserDef _ -> t
	| TyVar(nb) -> (
		try 
			let tt = (List.assoc nb subs) in 
				if List.length subs = 1 then tt else 
					ty_subst subs tt
		with
			| Not_found -> t
		)
	| TyArr x -> TyArr(ty_subst subs x )
	| TyFun(ps,q) -> TyFun(List.map (fun p -> ty_subst subs p) ps,ty_subst subs q)
	| TyTuple ps -> TyTuple(List.map (fun x -> ty_subst subs x) ps)

let rec constrs_subst s cs =
        match cs with
        | [] -> []
        | (x,y,d) :: xs -> (ty_subst [s] x,ty_subst [s] y,d) :: (constrs_subst s xs)

(* constrs_subst のとこでO(n^2) かかっていそう。 *)
let rec unify tyenv cs = 
	(* print_constrs cs; *)
	let self = unify tyenv in
	match cs with
	| [] -> []
	| (t1,t2,deb) :: xs -> if t1 == t2 then self xs else (
		(*
		print_string ((String.concat ";" (List.map (fun (a,b) -> a) tyenv)) ^ "\n"); 
		Printf.printf "%s @ %s\n"  (type2str t1) (type2str t2);
		*)
		try 
			match t1,t2 with
			| TyVar x,y | y,TyVar x -> (
					if ty_var_appear y x then 
						raise (TypeError(t1,t2,deb)) else 
						(x,y) :: (self (constrs_subst (x,y) xs)) 
				)
			| TyArr a,TyArr b -> self ((a,b,deb) :: xs)
			| TyFun(vs,b),TyFun(ws,d) -> ( (* 部分適用に対応する。 *)
					let rec f nvs nws = (
						match nvs,nws with
						| [],[] -> [(b,d,deb)]
						| [],rws -> [(b,TyFun(rws,d),deb)]
						| rvs,[] -> [(TyFun(rvs,b),d,deb)]
						| v :: rvs,w :: rws -> (v,w,deb) :: f rvs rws
					) in
				self ((f vs ws) @ xs)
				)
			| TyTuple ps,TyTuple qs -> self ((List.map2 (fun a b -> (a,b,deb)) ps qs) @ xs)
			(* 多相性のために追加する *)
			| TyNum,TyInt | TyInt,TyNum | TyNum,TyFloat | TyFloat,TyNum -> self xs
			| TyUserDef(a),TyUserDef(b) when a = b -> self xs
			| TyUserDef(a),b | b,TyUserDef(a) when List.mem_assoc a tyenv -> (
					self ((List.assoc a tyenv,b,deb) :: xs)
				)
			| _ -> raise (TypeError(t1,t2,deb))
		with 
			| Invalid_argument("List.map2") -> raise (TypeError(t1,t2,deb))
	)


let rec ast_subst subs (ast,(nt,deb)) = 
	let f = ast_subst subs in
	let mf = List.map f in
	let nf (x,(t,d)) = (x,(ty_subst subs t,d)) in
	let mnf = List.map nf in
	let tast = match ast with
	| TConst _ -> ast
	| TVar(na) -> TVar(nf na)
	| TOp(op,es) -> TOp(op,mf es)
	| TIf(e1,e2,e3) -> TIf(f e1,f e2,f e3)
	| TLet(na,e1,e2) -> TLet(nf na,f e1,f e2)
	| TLetRec(na,vs,e1,e2) -> TLetRec(nf na,mnf vs,f e1,f e2)
	| TApp(e1,es) -> TApp(f e1,mf es)
	| TTuple(es) -> TTuple(mf es)
	| TLetTuple(vs,e1,e2) -> TLetTuple(mnf vs,f e1,f e2)
	in
		(tast,(ty_subst subs nt,deb))

type ('a, 'b) either = Left of 'a | Right of 'b

let rec fix_partial_apply (ast,(t,deb)) =  
	let f = fix_partial_apply in
	let mf = List.map f in
	let tast = match ast with
	| TConst _ | TVar _ -> ast
	| TOp(op,es) -> TOp(op,mf es)
	| TIf(e1,e2,e3) -> TIf(f e1,f e2,f e3)
	| TLet(na,e1,e2) -> TLet(na,f e1,f e2)
	| TLetRec(na,vs,e1,e2) -> TLetRec(na,vs,f e1,f e2)
	| TApp(e1,e2) -> (
		let ((_,(t,d) as td) as ftd) = f e1 in
		let es = mf e2 in
		let raise_invalid () = 
			raise (Failure(Printf.sprintf "Invalid apply for %s with (%s) at %s" 
					(type2str t) (String.concat ", " (List.map (fun (_,(t,_)) -> (type2str t)) es)) (debug_data2str d)
			))
		in
		let rtes,rd = (
				match t with
				| TyFun(vs,rd) -> (
						(let rec f ntvs nes acc = (
							match ntvs,nes with
							| _,[] -> Left ntvs
							| [],_ -> Right(nes,acc)
							| v :: vs,e :: res -> f vs res (acc @ [e])
						) in f vs es []),rd
					)
				| _ -> raise_invalid ()
			) in
			(match rtes with
			| Left [] | Right([],_) -> TApp(ftd,es)
			| Right(res,acc) -> (
					let te1 = (TApp(e1,acc),(rd,deb)) in
					let tast,_ = fix_partial_apply (TApp(te1,res),(t,deb)) in
						tast
				)
			| Left rts -> (
					(* rtsが空でないなら、 App ftd es を、 let rec tmpf rts_1 rts_2 .. rts_n = ftd (es @ rts_1 rts_2 .. rts_n) in tmpf に変更する *)
					let tmpftd = (TyFun(rts,rd),deb) in
					let tmpf = (genvar (),tmpftd) in
					let tmpvs = List.map (fun t -> (genvar (),(t,deb))) rts in
					let te1 = TApp(ftd,es @ (List.map (fun ((_,td) as natd) -> ((TVar natd),td)) tmpvs)) in
					TLetRec(tmpf,tmpvs,(te1,tmpftd),(TVar(tmpf),tmpftd))
				)
			)
		)
	| TTuple(es) -> TTuple(mf es)
	| TLetTuple(vs,e1,e2) -> TLetTuple(vs,f e1,f e2)
	in
		(tast,(t,deb))


(*
型推論ができたなら、
単に全部 let in で結んでよいはず。
exprのみのやつは、 let _ = expr in にして。
*)







(* 
# type a = H of int;;
type a = H of int
# type b = a * int;;
type b = a * int
# type a = b * int;;
type a = b * int
# (H(3) : a);;
Error: This expression has type a/1016 but an expression was expected of type
         a/1020 = b * int
こういうシブイ話もあるが一旦無視していいよね...?
*)


type type_def = 
	| TDRename of ty
	| TDVariant of ((variant_tag * (ty list)) list)
	| TDNull

let variantenv () = []

let user_defined_type_env () = [
	("int",TyInt);
	("float",TyFloat)
]



let rec eval_variant defined_types te = 
	let f = eval_variant defined_types in
	match te with
	| ETInt -> TyInt
	| ETFloat -> TyFloat
	| ETVar na -> (
			if List.mem na defined_types then (TyUserDef na)
				else raise (Failure(Printf.sprintf "undefined type name %s" na))
		)
	| ETTuple ts -> TyTuple(List.map f ts)

(* 型定義に無限ループは存在しないはず 
# type a = b and b = a;;
Error: The type abbreviation a is cyclic
とか。
*)

let check ast = 
	try (
		let sv = (
			(fun x -> x),
			(genv ()),
			(variantenv ()),
			(user_defined_type_env ()),
			List.map fst (user_defined_type_env ())
		) in
		let astzero = (TConst(CInt(0)),(TyInt,default_debug_data)) in
		let tast,_,_,_,_ = List.fold_left (fun (f,env,venv,tyenv,defined_types) ast ->
			addglobalcs := [];
			match ast with
			| FExpr e -> ((
					let tast,tc,rt,_ = type_infer venv e env in
					(* print_type rt;
					print_constrs tc; *)
					let subs = unify tyenv (tc @ !addglobalcs) in
					(* print_subs subs; *)
					let rast = ast_subst subs tast in
					let sast = fix_partial_apply rast in
					(fun y -> f (TLet(("_",(rt,default_debug_data)),sast,y),(snd y)))
				),env,venv,tyenv,defined_types)
			| FDecl de -> (
					match de with
					| DLet(na,e) -> (
							let tast,tc,rt,_ = type_infer venv e env in
							print_string (texp2str tast);
							let subs = unify tyenv (tc @ !addglobalcs) in
							let rast = ast_subst subs tast in
							let trt = ty_subst subs rt in
							let sast = fix_partial_apply rast in
							(
								(fun y -> f (TLet((na,(trt,default_debug_data)),sast,y),(snd y))),
								((na,trt) :: env),
								venv,tyenv,defined_types
							)
						)
						
					| DVariant(na,ts) -> (
							let tts = List.map (fun (tg,te) -> (tg,List.map (fun x -> eval_variant (na :: defined_types) x) te)) ts in
							let ttts = List.mapi (fun i (tg,tes) -> 
								(tg,(i,TyUserDef(na),tes))
							) tts
							in
							(
								f,env,
								ttts @ venv,
								tyenv,
								na :: defined_types
							)
						)
					| _ -> raise (Failure("Unimplemented"))
				)
		) sv ast
		in tast astzero 
	) with
		| TypeError(t1,t2,deb) -> 
			raise (Failure(Printf.sprintf 	
				"Type Unify Failed:\n %s \n with type %s and %s" (Debug.debug_data2str deb) (type2str t1) (type2str t2)))
