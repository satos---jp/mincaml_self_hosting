open Syntax
open Debug
open Genint


let genvar () = Printf.sprintf "@pp_%d" (genint ())


(*
let (a,b) = e 

は

let @pp = e
let a = @pp[0]
let b = @pp[1]

で、

let rec f (a,b) = e

は

let f @pp = 
	let a = @pp[0] in
	let b = @pp[1] in
	e

で
*)

(* pattern から、 name * それにbindするためのexpr のlistを得る*)
let rec get_bind deb pa ne = 
	let self = get_bind deb in
	match pa with
	| PVar v -> [(v,ne)]
	| PTuple(ps) -> (
			let ls = List.length ps in
			let tps = List.mapi (fun i x -> self x (EOp(OGetTupleWithLen(ls,i),[ne]),deb)) ps in
			List.flatten tps
		)
	| _ -> raise (Failure ("invalid bind at " ^ (Debug.debug_data2str deb)))

let rec preprocess_expr astdeb = 
	let self = preprocess_expr in
	let ast,deb = astdeb in
	let gbf = get_bind deb in
	let tast = (
		match ast with
		| ELetRec(na,ps,e2,e3) -> (
				let te2 = self e2 in
				let te3 = self e3 in
				let nbs = List.map (fun p -> let v = genvar () in (v,gbf p ((EVar v),deb))) ps in
				let tps = List.map (fun (v,_) -> PVar v) nbs in
				let tte2 = List.fold_left (fun e (na,be) -> 
					ELet(na,be,e),deb
				)	te2 (List.flatten (List.map snd nbs)) in
				ELetRec(na,tps,tte2,te3)
			)
		| ELet(na,e2,e3) -> (
				let te2 = self e2 in
				let te3 = self e3 in
				ELet(na,te2,te3)
			)
		| ELetTuple(ns,e2,e3) -> ELetTuple(ns,self e2,self e3)
		| EConst _ | EVar _ -> ast
		| EOp(v,es) -> EOp(v,List.map self es)
		| EIf(e1,e2,e3) -> EIf(self e1, self e2, self e3)
		| EApp(e,es) -> EApp(self e,List.map self es)
		| ETuple(es) -> ETuple(List.map self es)
		| EMatch(e,pes) -> EMatch(self e,List.map (fun (p,e) -> (p,self e)) pes)
		| EVariant(tag,es) -> EVariant(tag,List.map self es)
	) in
	tast,deb


let preprocess_decl de =
	match de with
	| DLet(na,e) -> (
			let te = preprocess_expr e in
			[FDecl(DLet(na,te))] 
		)
	| DLetRec(na,vs,e) -> (
			let te = preprocess_expr e in
			[FDecl(DLetRec(na,vs,te))] 
		)
	| DTypeRename _ | DVariant _ | DOpen _ -> [FDecl(de)]

let preprocess asts = 
	List.flatten (List.map (fun x -> 
		match x with
		| FExpr e -> [FExpr(preprocess_expr e)]
		| FDecl d -> preprocess_decl d
	) asts)
