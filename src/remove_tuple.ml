open Knorm

let rec remove_tuple env ast = 
	let reccall = remove_tuple env in
	match ast with
	| KLetTuple(vs,((na,_) as nd),e1) -> (
		let te1 = reccall e1 in
		try 
			let nvs = List.assoc na env in
				List.fold_left2 (fun r -> fun a -> fun b -> 
					KLet(a,KVar(b),r)) te1 vs nvs
		with
			| Not_found -> KLetTuple(vs,nd,te1)
		)
	| KLet((na,_) as a,e1,e2) -> (
			let te1 = reccall e1 in
			let te2 = (
				match te1 with
				| KTuple(vs) -> remove_tuple ((na,vs) :: env) e2
				| _ -> reccall e2
			) in
			KLet(a,te1,te2)
		)
	| KIf(c,a,b,e1,e2) -> KIf(c,a,b,reccall e1,reccall e2)
	| KLetRec(fn,vs,e1,e2) -> KLetRec(fn,vs,reccall e1,reccall e2)
	| _ -> ast

let remove ast = remove_tuple [] ast


