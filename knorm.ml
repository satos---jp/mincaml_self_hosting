open Syntax
open Debug


open Type_checker
type name = string * (ty * debug_data)

(* デバッグ情報はいったん落とす(実装がつらい...) *) 

type kexp = 
	| KConst of Syntax.const
	| KOp        of Syntax.optype * (name list)
	| KIfEq      of name * name * kexp * kexp
	| KIfLeq     of name * name * kexp * kexp
	| KLet       of name * kexp * kexp
	| KVar       of name
	| KLetRec    of name * (name list) * kexp * kexp
	| KApp       of name * (name list)
	| KTuple     of (name list)
	| KLetTuple  of (name list) * name * kexp
(*
	| KArrCrt    of name * name
	| KArrRead   of name * name
	| KArrWrite  of name * name * name
*)


let genvar = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@k_%d" !c)

let rec knorm (ast,nt) = 
	match ast with
	| TConst x -> KConst x
	| TVar x -> KVar x
	| TOp(s,xs) -> (
			let vxs = List.map (fun e -> (e,genvar ())) xs in
			List.fold_right (fun ((ne,net),nv) -> fun r -> KLet((nv,net),knorm (ne,net),r))
			vxs (KOp(s,List.map (fun ((_,t),v) -> (v,t)) vxs)) 
		)
	| TIf((TOp(Oeq,[e1;e2]),opt),e3,e4) -> (
			let t1 = snd e1 in
			let t2 = snd e2 in
			let v1 = genvar () in
			let v2 = genvar () in
			let k3 = knorm e3 in
			let k4 = knorm e4 in
			KLet((v1,t1),knorm e1,
				KLet((v2,t2),knorm e2,
					KIfEq((v1,t1),(v2,t2),k3,k4)))
		)
	| TIf(e1,e2,e3) -> (
			let (_,(_,d)) = e1 in
			let tast = TIf(
				(TOp(Oeq,[e1;(TConst(CBool true),(TyBool,d))]),(TyBool,d))
				,e2,e3) in
			knorm (tast,nt)
		)
	| TLet(v1,e2,e3) -> KLet(v1,knorm e2,knorm e3)
	| TLetRec(v1,vs,e2,e3) -> KLetRec(v1,vs,knorm e2,knorm e3)
	| TApp(e1,vs) -> (
			let t1 = snd e1 in
			let v1 = genvar () in
			let vxs = List.map (fun e -> (e,genvar ())) vs in
			KLet((v1,t1),knorm e1,
				List.fold_right (fun ((ne,net),na) -> fun r -> KLet((na,net),knorm (ne,net),r))
				vxs (KApp((v1,t1),(List.map (fun ((_,t),v) -> (v,t)) vxs)))
			)
		)
	| TTuple es -> (
			let vxs = List.map (fun e -> (e,genvar ())) es in
			List.fold_left (fun r -> fun ((ne,net),nv) -> KLet((nv,net),knorm (ne,net),r))
			(KTuple(List.map (fun ((_,t),v) -> (v,t)) vxs)) vxs
		)
	| TLetTuple(v1,e2,e3) -> (
			let t2 = snd e2 in
			let v2 = genvar () in
			KLet((v2,t2),knorm e2,KLetTuple(v1,(v2,t2),knorm e3))
		)

