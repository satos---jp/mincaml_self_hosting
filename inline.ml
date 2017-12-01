open Knorm
open Alpha
open Main_option

let rec inline ast fns = 
	match ast with
	| KIf(op,a,b,e1,e2) -> KIf(op,a,b,inline e1 fns,inline e2 fns)
	| KLet(na,e1,e2) -> KLet(na,inline e1 fns,inline e2 fns)
	| KLetTuple(na,vs,e1) -> KLetTuple(na,vs,inline e1 fns)
	| KLetRec((fn,fdt),vs,e1,e2) -> (
			let tfns = if kexp_size e1 < 30 then 
				(Printf.printf "inlining %s\n" fn; (fn,(vs,e1)) :: fns)
			else fns
			in
			KLetRec((fn,fdt),vs,inline e1 tfns,inline e2 tfns)
		)
	| KApp((fn,fdt),vs) -> (
			try 
				let (tvs,te) = List.assoc fn fns in (
					try
						alpha_conv te (List.map2 (fun (a,_) -> fun (b,_) -> (a,b)) tvs vs)
					with
						| Invalid_argument("List.map2") -> 
							raise (Failure (Printf.sprintf "invalid Kapp at inline\n %s" (knorm2str ast)))
				)
			with
				| Not_found -> ast
		)
	| _ -> ast

let inliner ast = 
	ivprint "inlining";
	if !noinline then ast else inline ast []

	
	
	
	



