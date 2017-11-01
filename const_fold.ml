open Knorm


let rec const_fold env ast = 
	let reccall = const_fold env in
	match ast with
	| KOp(OGetTuple(i),[(na,_)]) -> (
			try 
				let vs = List.assoc na env in
					KVar(List.nth vs i)
			with
				| Not_found -> ast
		)
	| KLet((na,_) as natd,KTuple(vs),e1) -> KLet(natd,KTuple(vs),const_fold ((na,vs) :: env) e1)
	| _ -> kexp_recconv reccall ast

let folder ast = const_fold [] ast
