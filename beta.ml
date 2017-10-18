open Knorm

let rec beta_reduce ast env = 
	let conv_var (x,td) = 
		try 
			(List.assoc x env,td)
		with
			| Not_found -> (x,td)
	in
	let conv_list vs = List.map conv_var vs
	in
	match ast with
	| KConst _ -> ast
	| KOp(op,vs) -> KOp(op,conv_list vs)
	| KIf(op,a,b,e1,e2) -> KIf(op,conv_var a,conv_var b,beta_reduce e1 env,beta_reduce e2 env)
	| KLet((na,td),e1,e2) -> (
			match beta_reduce e1 env with
			| KVar((tna,_)) -> beta_reduce e2 ((na,tna) :: env)
			| te1 -> KLet((na,td),te1,beta_reduce e2 env)
		)
	| KVar(x) -> KVar(conv_var x)
	| KLetRec(na,vs,e1,e2) -> (
			KLetRec(na,vs,
				beta_reduce e1 env,
				beta_reduce e2 env
			)
		)
	| KApp(fn,vs) -> KApp(conv_var fn,conv_list vs)
	| KTuple(vs) -> KTuple(conv_list vs)
	| KLetTuple(vs,na,e1) -> (
			KLetTuple(vs,
				conv_var na,
				beta_reduce e1 env
			)
		)
	
let beter ast = beta_reduce ast []
