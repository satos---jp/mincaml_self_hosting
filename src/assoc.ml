open Knorm
open Main_option

let rec assoc_base ast = 
	let reccall = assoc_base in
	match ast with
	| KLet(na,e1,e2) -> (
			match e1 with
			| KLet(nb,be1,be2) -> reccall (KLet(nb,be1,KLet(na,be2,e2)))
			| KLetTuple(vs,nb,be1) -> KLetTuple(vs,nb,reccall (KLet(na,be1,e2)))
			| _ -> KLet(na,reccall e1,reccall e2)
		)
	| _ -> kexp_recconv reccall ast

let assoc ast = 
	ivprint "associng";
	assoc_base ast

