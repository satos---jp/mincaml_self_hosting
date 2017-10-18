open Syntax
open Knorm

type hash = Vs of int * (string list) | C of const | N of hash list 


let rec hasher ast = 
	match ast with
	| KConst c -> C c
	| KOp(op,vs) -> Vs(1,(op2str op) :: (List.map fst vs))
	| KIf(op,a,b,e1,e2) -> N([Vs(2,(comptype2str op) :: (List.map fst [a;b]));hasher e1;hasher e2])
	| KLet((na,_),e1,e2) -> N([Vs(3,[na]);hasher e1; hasher e2])
	| KVar((na,_)) -> Vs(4,[na])
	| KLetRec((fn,_),vs,e1,e2) -> N([Vs(5,fn :: (List.map fst vs));hasher e1;hasher e2])
	| KApp((na,_),vs) -> Vs(6,na :: (List.map fst vs))
	| KTuple(vs) -> Vs(7,List.map fst vs)
	| KLetTuple(vs,(na,_),e1) -> N([Vs(8,na :: (List.map fst vs));hasher e1])


let rec effect ast = 
	match ast with
	| KOp(OArrCrt,_) | KOp(OArrRead,_) | KOp(OArrWrite,_) | KApp _ -> true
	| KConst _ | KOp _ | KVar _ | KTuple _ -> false
	| KIf(_,_,_,e1,e2) | KLet(_,e1,e2) -> (effect e1) || (effect e2)
	| KLetRec(_,_,_,e1) | KLetTuple(_,_,e1) -> effect e1

let rec elim_sube ast env = 
	try 
		List.assoc (hasher ast) env
	with
		| Not_found -> (
			match ast with
			| KConst _ | KOp _ | KVar _ | KTuple _ | KApp _ -> ast
			| KIf(op,a,b,e1,e2) -> KIf(op,a,b,elim_sube e1 env,elim_sube e2 env)
			| KLetTuple(vs,na,e1) -> KLetTuple(vs,na,elim_sube e1 env)
			| KLet(na,e1,e2) -> (
					let te1 = elim_sube e1 env in
					let tenv = if effect te1 then env else ((hasher te1),KVar(na)) :: env in
						KLet(na,te1,elim_sube e2 tenv)
				)
			| KLetRec(na,vs,e1,e2) -> KLetRec(na,vs,elim_sube e1 env,elim_sube e2 env)
		)


let elimer ast = elim_sube ast []

