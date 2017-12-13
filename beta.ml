open Knorm

let rec beta_reduce letenv tupleenv ast = 
	let conv_var (x,td) = 
		try 
			(List.assoc x letenv,td)
		with
			| Not_found -> (x,td)
	in
	let conv_list vs = List.map conv_var vs
	in
	let rec reccall = beta_reduce letenv tupleenv in 
	match ast with
	| KConst _ -> ast
	| KOp(op,vs) -> KOp(op,conv_list vs)
	| KIf(op,a,b,e1,e2) -> KIf(op,conv_var a,conv_var b,reccall e1,reccall e2)
	| KLet((na,td),e1,e2) -> (
			let te1 = reccall e1 in
			match te1 with
			| KVar((tna,_)) -> beta_reduce ((na,tna) :: letenv) tupleenv e2
		(*
			| KTuple(vs) -> KLet((na,td),te1,beta_reduce letenv ((na,vs) :: tupleenv) e2)
		*)
			| _ -> ( 
				(* let x = e1 in x の形のを簡約しておく。(末尾再帰のために) *)
				let te2 = reccall e2 in
				match te2 with
				| KVar((tna,_)) when na = tna -> te1
				| _ -> KLet((na,td),te1,te2)
			)
		)
	| KVar(x) -> KVar(conv_var x)
	| KLetRec(na,vs,e1,e2) -> KLetRec(na,vs,reccall e1,reccall e2)
	| KApp(fn,vs) -> KApp(conv_var fn,conv_list vs)
	| KTuple(vs) -> KTuple(conv_list vs)
	| KLetTuple(vs,na,e1) -> KLetTuple(vs,conv_var na,reccall e1)
	
let beter ast = beta_reduce [] [] ast

