open Syntax
open Debug

type tyvar = int

type ty =
	| TyInt
	| TyFloat
	| TyBool
	| TyVar of tyvar
	| TyArr of ty
	| TyFun of ty * ty
	| TyTuple of ty list

exception TypeError of ty * ty * Debug.debug_data

let tyvar2str v = Printf.sprintf "'a%d" v

let rec type2str_with_pa t =
        match t with
        | TyInt | TyBool | TyFloat | TyVar _ -> type2str t
        | TyFun _ | TyArr _ | TyTuple _ -> "(" ^ (type2str t) ^ ")"

and type2str t =
	match t with
	| TyInt -> "int"
	| TyBool -> "bool"
	| TyFloat -> "float"
	| TyVar v -> tyvar2str v
	| TyFun (t1, t2) -> (
		Printf.sprintf "%s -> %s" 
			(type2str_with_pa t1) 
			(match t2 with | TyFun _  -> (type2str t2) | _ -> (type2str_with_pa t2))
		)
	| TyArr t -> Printf.sprintf "Array %s" (type2str_with_pa t)
	| TyTuple ts -> (
			match ts with
			| [] -> "()"
			| x :: xs -> 
				List.fold_left (fun r -> fun t -> r ^ " * " ^ (type2str_with_pa t)) (type2str_with_pa x) xs
		)

let print_type t = print_string ((type2str t) ^ "\n")


let genvar = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@t_%d" !c)

let gentype = let c = ref 0 in (fun () -> c := (!c)+1; TyVar(!c))


let ops = [
	(Onot,[TyInt],TyInt);
	(Ominus,[TyInt],TyInt);
	(Oadd,[TyInt;TyInt],TyInt);
	(Osub,[TyInt;TyInt],TyInt);
	(Omul,[TyInt;TyInt],TyInt);
	(Odiv,[TyInt;TyInt],TyInt);
	(Ofadd,[TyFloat;TyFloat],TyFloat);
	(Ofsub,[TyFloat;TyFloat],TyFloat);
	(Ofmul,[TyFloat;TyFloat],TyFloat);
	(Ofdiv,[TyFloat;TyFloat],TyFloat);
	(Oeq,[TyInt;TyInt],TyInt);
	(Oneq,[TyInt;TyInt],TyInt);
	(Olt,[TyInt;TyInt],TyInt);
	(Oleq,[TyInt;TyInt],TyInt);
	(Ogt,[TyInt;TyInt],TyInt);
	(Ogeq,[TyInt;TyInt],TyInt);
]

let genv = [
	("false",TyInt);
	("fiszero",TyFun(TyFloat,TyInt));
	("fispos",TyFun(TyFloat,TyInt));
	("fisneg",TyFun(TyFloat,TyInt));
	("fless",TyFun(TyFloat,TyFun(TyFloat,TyInt)));
	("fabs",TyFun(TyFloat,TyFloat));
	("floor",TyFun(TyFloat,TyFloat));
	("fsqr",TyFun(TyFloat,TyFloat)); (* x -> x^2 のほう *)
	("fneg",TyFun(TyFloat,TyFloat));
	("fhalf",TyFun(TyFloat,TyFloat));
	("sqrt",TyFun(TyFloat,TyFloat));   (* x -> root x のほう *)
	("sin",TyFun(TyFloat,TyFloat));
	("cos",TyFun(TyFloat,TyFloat));
	("atan",TyFun(TyFloat,TyFloat));
	("int_of_float",TyFun(TyFloat,TyInt));
	("float_of_int",TyFun(TyInt,TyFloat));
	("read_int",TyFun(TyTuple([]),TyInt));
	("read_float",TyFun(TyTuple([]),TyFloat));
	("print_char",TyFun(TyInt,TyTuple([])));
	("print_int",TyFun(TyInt,TyTuple([])));
]


(* astdebは、 ast, deb の組。 *)
(* 環境は、 (旧変数名,(新変数名,型)) *)
(* 返り値は、 (新ast,制約,型,debug情報) *)
(* 制約は、型1,型2,デバッグ情報 *)
let rec type_infer astdeb env = 
	let ast,deb = astdeb in
	let rast,rc,rt = (
	match ast with
	| EConst(CBool x) -> (ast,[],TyBool)
	| EConst(CFloat x) -> (ast,[],TyFloat)
	| EConst(CInt x) -> (ast,[],TyInt)
	| EVar(x) -> (
			try (
				let tv,tt = List.assoc x env in
				(EVar(tv),[],tt)
			)
			with
				| Not_found -> raise (Failure("undefined variable " ^ x))
		)
(*
	| EOp("add" as s,[e1;e2]) | EOp("sub" as s,[e1;e2]) | EOp("mul" as s,[e1;e2]) | EOp("div" as s,[e1;e2]) -> (
			let te1,c1,tt1 = type_infer e1 env in
			let te2,c2,tt2 = type_infer e2 env in
				(EOp(s,[te1;te2]),(tt1,TyInt) :: (tt2,TyInt) :: c1 @ c2,TyInt)
		)
*)
	| EOp(Osemi2,[e1;e2]) -> (
			let te1,c1,tt1,deb1 = type_infer e1 env in
			let te2,c2,tt2,deb2 = type_infer e2 env in
				(EOp(Osemi2,[te1;te2]),(tt1,TyTuple([]),deb1) :: c1 @ c2,tt2)
		)
	| EOp(Osemi1,[e1]) -> (
			let te1,c1,tt1,deb1 = type_infer e1 env in
				(EOp(Osemi1,[te1]),c1,tt1)
		)
(* Arr系のやつ、ぜんぶOpにしてしまっていいきがする*)
	| EOp(OArrCrt,[e1;e2]) -> (
			let te1,c1,tt1,deb1 = type_infer e1 env in
			let te2,c2,tt2,_ = type_infer e2 env in
				(EOp(OArrCrt,[te1;te2]),(tt1,TyInt,deb1) :: c1 @ c2,TyArr(tt2))
		)
	| EOp(OArrRead,[e1;e2]) -> (
			let te1,c1,tt1,deb1 = type_infer e1 env in
			let te2,c2,tt2,deb2 = type_infer e2 env in
			let rt = gentype () in
				(EOp(OArrRead,[te1;te2]),(tt1,TyArr(rt),deb1) :: (tt2,TyInt,deb2) :: c1 @ c2,rt)
		)
	| EOp(OArrWrite,[e1;e2;e3]) -> (
			let te1,c1,tt1,_ = type_infer e1 env in
			let te2,c2,tt2,deb2 = type_infer e2 env in
			let te3,c3,tt3,deb3 = type_infer e3 env in
				(EOp(OArrWrite,[te1;te2;te3]),(tt1,TyArr(tt3),deb3) :: (tt2,TyInt,deb2) :: c1 @ c2 @ c3,TyTuple([]))
		)
	| EOp(s,vs) -> (
			let tects = List.map (fun x -> type_infer x env) vs in
			let tes = List.map (fun (x,_,_,_) -> x) tects in
			let tcs = List.concat (List.map (fun (_,x,_,_) -> x) tects) in
			let tts = List.map (fun (_,_,x,_) -> x) tects in 
			try 
				let na,nts,rt = List.find (fun (x,_,_) -> x = s) ops in
					(EOp(s,tes),(List.map2 (fun a b -> (a,b,deb)) nts tts) @ tcs,rt)
			with
				| Not_found -> raise (Failure ("operand " ^ (op2str s) ^ " doesn't exist"))
		)
	| EIf(e1,e2,e3) -> (
			let te1,c1,tt1,deb1 = type_infer e1 env in
			let te2,c2,tt2,_ = type_infer e2 env in
			let te3,c3,tt3,deb3 = type_infer e3 env in
				(EIf(te1,te2,te3),(tt1,TyInt,deb1) :: (tt2,tt3,deb3) :: c1 @ c2 @ c3,tt2)
		)
	| ELet(n1,e2,e3) -> (
			let tn1 = (genvar ()) ^ "_" ^ n1 in
			let te2,c2,tt2,_ = type_infer e2 env in
			let te3,c3,tt3,_ = type_infer e3 ((n1,(tn1,tt2)) :: env) in
			(ELet(tn1,te2,te3),c2 @ c3,tt3)
		)
	| ELetRec(f1,ns,e2,e3) -> (
			(* fのα変換 *)
			let fn1 = (genvar ()) ^ "_" ^ f1 in
			(* fの返り値型 *)
			let fn1rt = gentype () in
			(* nsのα変換と、 型変数を作ったもの *)
			let tns = List.map (fun x -> (x,(genvar (),gentype ()))) ns in
			(* fの型 *)
			let f1t = List.fold_right (fun x -> fun r -> TyFun(snd (snd x),r)) tns fn1rt in
			(* e2についての推論 *) 
			let te2,c2,tt2,deb2 = type_infer e2 ((f1,(fn1,f1t)) :: tns @ env) in
			(* e3についての推論 *)
			let te3,c3,tt3,_ = type_infer e3 ((f1,(fn1,f1t)) :: env) in
			(ELetRec(fn1,List.map (fun (_,(x,_)) -> x) tns,te2,te3),(tt2,fn1rt,deb2) :: c2 @ c3,tt3)
		)
	| EApp(e1,e2) -> (
		(* カリー化できないのでがんばる *)
			let te1,c1,tt1,deb1 = type_infer e1 env in
			let rt = gentype () in
			let res,rts,rc = 
				List.fold_right (fun ne -> fun (tes,apps,nrc) ->
				let te,tc,tt,_ = type_infer ne env in
					(te :: tes,TyFun(tt,apps),tc @ nrc)) e2 ([],rt,[]) in
				(EApp(te1,res),(tt1,rts,deb1) :: c1 @ rc,rt)
		)
	| ETuple(et) -> (
		(* 制約集合の和をとるだけにする。 *)
			let tet = List.map (fun x -> type_infer x env) et in
				(ETuple(List.map (fun (x,_,_,_) -> x) tet),
				 List.concat (List.map (fun (_,x,_,_) -> x) tet),
				 TyTuple(List.map (fun (_,_,x,_) -> x) tet))
		)
	| ELetTuple(ns,e1,e2) -> (
			let te1,c1,tt1,deb1 = type_infer e1 env in
			let tns = List.map (fun x -> (x,(genvar (),gentype ()))) ns in
			let te2,c2,tt2,_ = type_infer e2 (tns @ env) in
				(ELetTuple(List.map (fun (_,(x,_)) -> x) tns,te1,te2),
				(tt1,TyTuple(List.map (fun (_,(_,x)) -> x) tns),deb1) :: c1 @ c2,tt2)
		)
	) in
	((rast,deb),rc,rt,deb)

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

let rec ty_var_appear t v =
        match t with
        | TyInt | TyBool | TyFloat -> false
        | TyFun (t1, t2) -> (ty_var_appear t1 v) || (ty_var_appear t2 v)
        | TyVar x -> x = v
        | TyArr t -> (ty_var_appear t v)
        | TyTuple ts -> List.fold_left (fun r -> fun t -> r || (ty_var_appear t v)) false ts

let rec ty_subst (na,tt) t = 
	match t with
	| TyInt | TyFloat | TyBool -> t
	| TyVar(nb) -> if na == nb then tt else t
	| TyArr x -> TyArr(ty_subst (na,tt) x )
	| TyFun(p,q) -> TyFun(ty_subst (na,tt) p,ty_subst (na,tt) q)
	| TyTuple ps -> TyTuple(List.map (fun x -> ty_subst (na,tt) x) ps) 

let rec constrs_subst s cs =
        match cs with
        | [] -> []
        | (x,y,d) :: xs -> (ty_subst s x,ty_subst s y,d) :: (constrs_subst s xs)

let rec unify cs = 
	(* print_constrs cs; *)
	match cs with
	| [] -> []
	| (t1,t2,deb) :: xs -> if t1 == t2 then unify xs else (
		match t1,t2 with
		| TyVar x,y | y,TyVar x -> (
				if ty_var_appear y x then 
					raise (TypeError(t1,t2,deb)) else 
					unify (constrs_subst (x,y) xs)
			)
		| TyArr a,TyArr b -> unify ((a,b,deb) :: xs)
		| TyFun(a,b),TyFun(c,d) -> unify ((a,c,deb) :: (b,d,deb) :: xs)
		| TyTuple ps,TyTuple qs -> unify ((List.map2 (fun a b -> (a,b,deb)) ps qs) @ xs)
		| _ -> raise (TypeError(t1,t2,deb))
	)



let check ast = 
	try (
		let tast,tc,rt,_ = type_infer ast (List.map (fun (a,b) -> (a,(a,b))) genv) in
		(* print_type rt;
		print_constrs tc; *)
		let _ = unify tc in
			tast
	) with
		| TypeError(t1,t2,deb) -> 
			raise (Failure(Printf.sprintf 	
				"Type Unify Failed:\n %s \n with type %s and %s" (Debug.data2str deb) (type2str t1) (type2str t2)))
