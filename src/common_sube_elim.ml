open Syntax
open Knorm



let rec effect ast = 
	match ast with
	| KOp(OArrCrt,_) | KOp(OArrRead,_) | KOp(OArrWrite,_) | KOp(OiArrRead(_),_) | KOp(OiArrWrite(_),_) | KApp _ -> true
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
					(* KConst‚Íí‚ç‚È‚¢B *)
					let tenv = if effect te1 then env else (
						match te1 with
						| KConst _ -> env
						| _ -> ((hasher te1),KVar(na)) :: env
					) 
					in
						KLet(na,te1,elim_sube e2 tenv)
				)
			(* letrec‚ð’´‚¦‚Äelim‚·‚é‚ÆAŒãX closure•ÏŠ·‚Å‚Â‚ç‚­‚È‚é!!  *)
			| KLetRec(na,vs,e1,e2) -> (
				KLetRec(na,vs,elim_sube e1 [],elim_sube e2 env)
			)
		)


let elimer ast = elim_sube ast []

