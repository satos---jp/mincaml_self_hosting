open Syntax

type name = string 

type kexp = 
  | KConst of Syntax.const
  | KOp        of string * (name list)
  | KIfEq      of name * name * kexp * kexp
  | KIfLeq     of name * name * kexp * kexp
	| KLet       of name * kexp * kexp
  | KVar       of name
  | KLetRec    of name * (name list) * kexp * kexp
  | KApp       of name * name
  | KTuple     of (name list)
  | KLetTuple  of (name list) * name * kexp
  | KArrCrt    of name * name
  | KArrRead   of name * name
  | KArrWrite  of name * name * name

let genvar = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@k_%d" !c)

let rec knorm ast = 
	match ast with
	| EConst x -> KConst x
	| EVar x -> KVar x
	| EOp(s,xs) -> (
			let vxs = List.map (fun e -> (e,genvar ())) xs in
			List.fold_left (fun r -> fun (ne,nv) -> KLet(nv,knorm ne,r))
			(KOp(s,List.map snd vxs)) vxs
		)
	| EIf(EOp("eq",[e1;e2]),e3,e4) -> (
			let v1 = genvar () in
			let v2 = genvar () in			
			let k3 = knorm e3 in
			let k4 = knorm e4 in
			KLet(v1,knorm e1,
				KLet(v2,knorm e2,
					KIfEq(v1,v2,k3,k4)))
		)
	| EIf(e1,e2,e3) -> (
			knorm (EIf(EOp("eq",[e1;EConst(CBool true)]),e2,e3))
		)
	| ELet(v1,e2,e3) -> KLet(v1,knorm e2,knorm e3)
	| ELetRec(v1,vs,e2,e3) -> KLetRec(v1,vs,knorm e2,knorm e3)
	| EApp(e1,e2) -> (
			let v1 = genvar () in
			let v2 = genvar () in			
			KLet(v1,knorm e1,
				KLet(v2,knorm e2,
					KApp(v1,v2)))
		)
	| ETuple es -> (
			let vxs = List.map (fun e -> (e,genvar ())) es in
			List.fold_left (fun r -> fun (ne,nv) -> KLet(nv,knorm ne,r))
			(KTuple(List.map snd vxs)) vxs
		)
	| ELetTuple(v1,e2,e3) -> (
			let v2 = genvar () in
			KLet(v2,knorm e2,KLetTuple(v1,v2,knorm e3))
		)
	| EArrCrt(e1,e2) -> (
			let v1 = genvar () in
			let v2 = genvar () in			
			KLet(v1,knorm e1,
				KLet(v2,knorm e2,
					KArrCrt(v1,v2)))
		)
	| EArrRead(e1,e2) -> (
			let v1 = genvar () in
			let v2 = genvar () in			
			KLet(v1,knorm e1,
				KLet(v2,knorm e2,
					KArrRead(v1,v2)))
		)
	| EArrWrite(e1,e2,e3) -> (
			let v1 = genvar () in
			let v2 = genvar () in			
			let v3 = genvar () in			
			KLet(v1,knorm e1,
				KLet(v2,knorm e2,
					KLet(v3,knorm e3,
						KArrWrite(v1,v2,v3))))
		)


