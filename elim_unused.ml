open Knorm
open Syntax

(* ast’†‚Ìenv‚É‚È‚¢•Ï”‚ªfv‚Å‚ ‚é *)
let rec get_fvs ast (env : string list) = 
	let filter vs = List.filter (fun x -> not (List.mem x env)) (List.map fst vs) in
	match ast with
	| KConst _ -> []
	| KOp(_,vs) | KTuple vs -> filter vs
	| KIf(_,x,y,e1,e2) -> (filter [x;y]) @ (get_fvs e1 env) @ (get_fvs e2 env)
	| KLet((x,_),e1,e2) -> (get_fvs e1 env) @ (get_fvs e2 (x :: env))
	| KLetRec((na,_),vs,e1,e2) -> (
			(get_fvs e1 (na :: (List.map fst vs) @ env)) @ (get_fvs e2 (na :: env)) 
			(* ŠÖ”–¼‚Íglobal‚É‚È‚é‚Ì‚Å‚Á‚Ä‚¨‚­ -> ‚Ù‚ñ‚Ü‚©H(‚¢‚ç‚ñ‚©‚Á‚½ -> ‚Æ‚ê‚Æ‚ç‚ñ‚â‚ñ‚¯ )  *)
		)
	| KApp(f,vs) -> filter (f :: vs)
	| KLetTuple(vs,tp,e1) -> (filter [tp]) @ (get_fvs e1 ((List.map fst vs) @ env))
	| KVar x -> (filter [x])

let rec effect ast = 
	match ast with
	| KOp(OArrCrt,_) | KOp(OArrWrite,_) | KApp _ -> true
	| KConst _ | KOp _ | KVar _ | KTuple _ -> false
	| KIf(_,_,_,e1,e2) | KLet(_,e1,e2) -> (effect e1) || (effect e2)
	| KLetRec(_,_,_,e1) | KLetTuple(_,_,e1) -> effect e1


let rec elim_unused ast = 
	let reccall = elim_unused in
	match ast with
	| KLet((a,_) as ad,e1,e2) -> (
		if effect e1 || List.mem a (get_fvs e2 []) then 
			KLet(ad,reccall e1,reccall e2) else reccall e2
		)
	| KLetTuple(vs,na,e2) -> (
		if effect e2 || List.exists (fun (x,_) -> List.mem x (get_fvs e2 [])) vs then 
			KLetTuple(vs,na,reccall e2) else reccall e2
		)
	| KLetRec((a,_) as ad,vs,e1,e2) -> (
		if List.mem a (get_fvs e2 []) then 
			KLetRec(ad,vs,reccall e1,reccall e2) else reccall e2
		)
	| KIf(a,b,c,e1,e2) -> KIf(a,b,c,reccall e1,reccall e2)
	| _ -> ast
