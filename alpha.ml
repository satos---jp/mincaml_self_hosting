open Knorm

let genname = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@a_%d" !c)

let rec alpha_conv ast env  = 
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
	| KIf(op,a,b,e1,e2) -> KIf(op,conv_var a,conv_var b,alpha_conv e1 env,alpha_conv e2 env)
	| KLet((na,td),e1,e2) -> (
			let tna = genname () in 
			KLet((tna,td),alpha_conv e1 env,alpha_conv e2 ((na,tna) :: env))
		)
	| KVar(x) -> KVar(conv_var x)
	| KLetRec((na,td),vs,e1,e2) -> (
			let tna = (genname ()) ^ na in 
			let tvs = List.map (fun _ -> genname ()) vs in
			KLetRec((tna,td),
				List.map2 (fun a -> fun (_,b) -> (a,b)) tvs vs,
				alpha_conv e1 ((na,tna) :: (List.map2 (fun (a,_) -> fun b -> (a,b)) vs tvs) @ env),
				alpha_conv e2 ((na,tna) :: env)
			)
		)
	| KApp(fn,vs) -> KApp(conv_var fn,conv_list vs)
	| KTuple(vs) -> KTuple(conv_list vs)
	| KLetTuple(vs,na,e1) -> (
			let tvs = List.map (fun _ -> genname ()) vs in
			KLetTuple(List.map2 (fun (_,ad) -> fun b -> (b,ad)) vs tvs,
				conv_var na,
				alpha_conv e1 ((List.map2 (fun (a,_) -> fun b -> (a,b)) vs tvs) @ env)
			)
		)

