open Syntax

exception TypeError
type tyvar = int

type ty =
  | TyInt
  | TyFloat
  | TyBool
  | TyArr of ty
  | TyFun of ty * ty
  | TyVar of tyvar
  | TyTuple of ty list


let genvar = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@t_%d" !c)

let gentype = let c = ref 0 in (fun () -> c := (!c)+1; TyVar(!c))


let ops = [
	("not",[TyInt],TyInt);
	("minus",[TyInt],TyInt);
	("add",[TyInt;TyInt],TyInt);
	("sub",[TyInt;TyInt],TyInt);
	("mul",[TyInt;TyInt],TyInt);
	("div",[TyInt;TyInt],TyInt);
	("fadd",[TyFloat;TyFloat],TyFloat);
	("fsub",[TyFloat;TyFloat],TyFloat);
	("fmul",[TyFloat;TyFloat],TyFloat);
	("fdiv",[TyFloat;TyFloat],TyFloat);
	("eq",[TyInt;TyInt],TyInt);
	("neq",[TyInt;TyInt],TyInt);
	("lt",[TyInt;TyInt],TyInt);
	("leq",[TyInt;TyInt],TyInt);
	("gt",[TyInt;TyInt],TyInt);
	("geq",[TyInt;TyInt],TyInt);
]

(* 環境は、 (旧変数名,(新変数名,型)) *)
(* 返り値は、 (新ast,制約,型) *)
let rec type_infer ast env = 
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
	| EOp("semi",[e1;e2]) -> (
			let te1,c1,tt1 = type_infer e1 env in
			let te2,c2,tt2 = type_infer e2 env in
				(EOp("semi",[te1;te2]),(tt1,TyTuple([])) :: c1 @ c2,tt2)
		)
	| EOp("semi",[e1]) -> (
			let te1,c1,tt1 = type_infer e1 env in
				(EOp("semi",[te1]),c1,tt1)
		)
	| EOp(s,vs) -> (
			let tects = List.map (fun x -> type_infer x env) vs in
			let tes = List.map (fun (x,_,_) -> x) tects in
			let tcs = List.concat (List.map (fun (_,x,_) -> x) tects) in
			let tts = List.map (fun (_,_,x) -> x) tects in 
			try 
				let na,nts,rt = List.find (fun (x,_,_) -> x = s) ops in
					(EOp(s,tes),(List.map2 (fun a b -> (a,b)) nts tts) @ tcs,rt)
			with
				| Not_found -> raise (Failure ("operand " ^ s ^ " doesn't exist"))
		)
	| EIf(e1,e2,e3) -> (
			let te1,c1,tt1 = type_infer e1 env in
			let te2,c2,tt2 = type_infer e2 env in
			let te3,c3,tt3 = type_infer e3 env in
				(EIf(te1,te2,te3),(tt1,TyInt) :: (tt2,tt3) :: c1 @ c2 @ c3,tt2)
		)
	| ELet(n1,e2,e3) -> (
			let tn1 = genvar () in
			let te2,c2,tt2 = type_infer e2 env in
			let te3,c3,tt3 = type_infer e3 ((n1,(tn1,tt2)) :: env) in
			(ELet(tn1,te2,te3),c2 @ c3,tt3)
		)
	| ELetRec(f1,ns,e2,e3) -> (
			(* fのα変換 *)
			let fn1 = genvar () in
			(* fの返り値型 *)
			let fn1rt = gentype () in
			(* nsのα変換と、 型変数を作ったもの *)
			let tns = List.map (fun x -> (x,(genvar (),gentype ()))) ns in
			(* fの型 *)
			let f1t = List.fold_left (fun r -> fun x -> TyFun(snd (snd x),r)) fn1rt tns in
			(* e2についての推論 *) 
			let te2,c2,tt2 = type_infer e2 ((f1,(fn1,f1t)) :: tns @ env) in
			(* e3についての推論 *)
			let te3,c3,tt3 = type_infer e3 ((f1,(fn1,f1t)) :: env) in
			(ELet(fn1,te2,te3),(tt2,fn1rt) :: c2 @ c3,tt3)
		)
	| EApp(e1,e2) -> (
			let te1,c1,tt1 = type_infer e1 env in
			let te2,c2,tt2 = type_infer e2 env in
			let rt = gentype () in
				(EApp(te1,te2),(tt1,TyFun(tt2,rt)) :: c1 @ c2,rt)
		)
	| ETuple(et) -> (
			let tet = List.map (fun x -> type_infer x env) et in
				(ETuple(List.map (fun (x,_,_) -> x) tet),
				 List.concat (List.map (fun (_,x,_) -> x) tet),
				 TyTuple(List.map (fun (_,_,x) -> x) tet))
		)
	| ELetTuple(ns,e1,e2) -> (
			let te1,c1,tt1 = type_infer e1 env in
			let tns = List.map (fun x -> (x,(genvar (),gentype ()))) ns in
			let te2,c2,tt2 = type_infer e2 (tns @ env) in
				(ELetTuple(List.map (fun (_,(x,_)) -> x) tns,te1,te2),
				(tt1,TyTuple(List.map (fun (_,(_,x)) -> x) tns)) :: c1 @ c2,tt2)
		)
	| EArrCrt(e1,e2) -> (
			let te1,c1,tt1 = type_infer e1 env in
			let te2,c2,tt2 = type_infer e2 env in
				(EArrCrt(te1,te2),(tt1,TyInt) :: c1 @ c2,TyArr(tt2))
		)
	| EArrRead(e1,e2) -> (
			let te1,c1,tt1 = type_infer e1 env in
			let te2,c2,tt2 = type_infer e2 env in
			let rt = gentype () in
				(EArrCrt(te1,te2),(tt1,TyInt) :: (tt2,TyArr(rt)) :: c1 @ c2,rt)
		)
	| EArrWrite(e1,e2,e3) -> (
			let te1,c1,tt1 = type_infer e1 env in
			let te2,c2,tt2 = type_infer e2 env in
			let te3,c3,tt3 = type_infer e3 env in
				(EArrWrite(te1,te2,te3),(tt1,TyArr(tt3)) :: (tt2,TyInt) :: c1 @ c2 @ c3,TyTuple([]))
		)


let rec subst t na tt = 
	match t with
	| TyInt | TyFloat | TyBool -> t
	| TyVar(nb) -> if na == nb then tt else t
	| TyArr x -> TyArr(subst x na tt)
	| TyFun(p,q) -> TyFun(subst p na tt,subst q na tt)
	| TyTuple ps -> TyTuple(List.map (fun x -> subst x na tt) ps) 


let rec unify cs = 
	match cs with
	| [] -> []
	| (t1,t2) :: xs -> if t1 == t2 then unify xs else
		match t1,t2 with
		| TyVar x,y | y,TyVar x -> xs
		| TyArr a,TyArr b -> unify ((a,b) :: xs)
		| TyFun(a,b),TyFun(c,d) -> unify ((a,c) :: (b,d) :: xs)
		| TyTuple ps,TyTuple qs -> unify ((List.map2 (fun a b -> (a,b)) ps qs) @ xs)
		| _ -> raise TypeError


let genv = [
	("false",TyInt);
	("e",TyFloat);
	("fiszero",TyFun(TyFloat,TyInt));
	("fispos",TyFun(TyFloat,TyInt));
	("fisneg",TyFun(TyFloat,TyInt));
	("fless",TyFun(TyFloat,TyInt));
	("fabs",TyFun(TyFloat,TyInt));
	("floor",TyFun(TyFloat,TyInt));
	("fsqr",TyFun(TyFloat,TyInt));
	("fneg",TyFun(TyFloat,TyInt));
	("fhalf",TyFun(TyFloat,TyInt));
	("sqrt",TyFun(TyFloat,TyInt));
	("sin",TyFun(TyFloat,TyFloat));
	("cos",TyFun(TyFloat,TyFloat));
	("atan",TyFun(TyFloat,TyFloat));
	("int_of_float",TyFun(TyFloat,TyInt));
	("float_of_int",TyFun(TyInt,TyFloat));
	("read_int",TyFun(TyTuple([]),TyInt));
	("read_float",TyFun(TyTuple([]),TyFloat));
	("print_char",TyFun(TyInt,TyTuple([])));
	("print_int",TyFun(TyInt,TyTuple([])));
	("and_net",TyArr(TyArr(TyInt)));
	("or_net",TyArr(TyArr(TyInt)));
]

let check ast = 
	let tast,tc,_ = type_infer ast (List.map (fun (a,b) -> (a,(a,b))) genv) in
	let _ = unify tc in
		tast


