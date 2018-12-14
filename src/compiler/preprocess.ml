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


let unfold_let_bind astdeb = 
	let ast,deb = astdeb in
	let gbf = get_bind deb in
	let tast = 
		match ast with
		| ELetRec([na,ps,e2],e3) -> (
				let nbs = List.map (fun p -> let v = genvar () in (v,gbf p ((EVar v),deb))) ps in
				let tps = List.map (fun (v,_) -> PVar v) nbs in
				let te2 = List.fold_left (fun e (na,be) -> 
					ELet(na,be,e),deb
				)	e2 (List.flatten (List.map snd nbs)) in
				ELetRec([na,tps,te2],e3)
			)
		| _ -> ast
	in
		tast,deb



(*
	let rec f1 x1 ... = e1
	and f2 y1 ... = e2
	and f3 z1 ... = e3
	を、	
	let rec tf1 tf2 tf3 x1 = ...
		let f1 = tf1 tf2 tf3 in
		let f2 = tf2 tf3 in
		let f3 = tf3 in
			e1
	let rec tf2 tf3 y1 = 
		let f1 = tf1 tf2 tf3 in
		let f2 = tf2 tf3 in
		let f3 = tf3 in 
			e2
	let rec tf3 z1 = 
		let f1 = tf1 tf2 tf3 in
		let f2 = tf2 tf3 in
		let f3 = tf3 in 
			e3
	
	let f1 = tf1 tf2 tf3 in
	let f2 = tf2 tf3 in
	let f3 = tf3 in 
	にします
*)

(*
let unfold_let_rec_and astdeb = 
	let ast,deb = astdeb in
	let gbf = get_bind deb in
	let tast = 
		match ast with
		| ELetRec([na,ps,e],e2) -> ast
		| ELetRec(napse,e2) -> (
				let nas = List.map (fun (x,_,_) -> x) napse in
				let tnas = List.mapi (fun i x -> (genvar ()) ^ (Printf.sprintf "_tf_%d_" i) ^ x) nas in
				let _,add_tf = List.fold_left (fun (fns,rf) (tna,na) -> 
					let etna = EVar(tna),deb in
					let appe = if List.length fns > 0 then EApp(etna,fns),deb else etna in
					let trf = fun e -> ELet(na,appe,rf e),deb in
					(etna :: fns,trf)
				) ([],(fun e -> e)) (List.combine tnas nas) in
				
				let tnapse = List.map2 (fun tna (_,ps,e) -> (tna,ps,e)) tnas napse in
				let basee = add_tf e2 in
				let _,(res,_) = List.fold_left (fun (fns,r) (tna,ps,e) -> 
					PVar(tna) :: fns,(ELetRec([
					tna,fns @ ps,
					add_tf e],r),deb)
				) ([],basee) tnapse 
				in
				res
			)
		| _ -> ast
	in
		tast,deb
*)

(*
	上のやつだと 15くらいの長さで型が爆発してしまう。
*)



(*
	let rec f1 x1 ... = e1
	and f2 y1 ... = e2
	and f3 z1 ... = e3
	を、	
	let rec fs tag = 
		let fk d = match fs Fk with Rk a -> a d in
		match tag with
		| F1 -> R1(fun x1 ... -> e1)
		| F2 -> R2(fun y1 ... -> e2)
		
	let f1 x1 ... = match fs F1 with R1 t -> t x1 ...
	let f2 y1 ... = match fs F2 with R2 t -> t y1 ...
	
	にします
	(Pattern match not exaustive が残るが...)
*)


(* let rec f x = g y in f がgと等しくなるように x に対して y を定める*)
let rec pattern2expr deb pa = 
	let self = pattern2expr deb in
	match pa with
	| PVar v -> EVar(v),deb
	| _ -> raise (Failure ("invalid pattern2expr at " ^ (Debug.debug_data2str deb)))



let unfold_let_rec_and astdeb = 
	let ast,deb = astdeb in
	let gbf = get_bind deb in
	let eapp a b = (EApp(a,b),deb) in
	let evar a = EVar(a),deb in
	let tast = 
		match ast with
		| ELetRec([na,ps,e],e2) -> ast
		| ELetRec(napse,e2) -> (
				let fsn = genvar () in
				let tagvar = genvar () in
				let napsefrtag = List.mapi (fun i (na,ps,e) -> (na,ps,e,Printf.sprintf "@Func%d" i,Printf.sprintf "@Retval%d" i)) napse in
								
				ELetRec([fsn,[PVar(tagvar)],
					List.fold_left (fun r (na,ps,e,ftag,rtag) -> 
						let lazyv = genvar () in
						let elazyv = EVar(lazyv),deb in
						let patfn = genvar () in
						let epatfn = EVar(patfn),deb in
						(ELetRec([na,[PVar(lazyv)],
							(EMatch(
								(eapp (evar fsn) [EVariant(ftag,[]),deb]),
								[[PVariantApp(rtag,PVar(patfn))],(eapp epatfn [elazyv])]
							),deb)],
						r),deb)
					) (
						EMatch(
							(EVar(tagvar),deb),
							List.map (fun (na,ps,e,ftag,rtag) -> 
								let dummyfn = genvar () in
								([PVariant(ftag)],
								(EVariant(rtag,[ELetRec([dummyfn,ps,e],(EVar(dummyfn),deb)),deb]),deb))
							) napsefrtag
						),deb
					) napsefrtag
				],(
					List.fold_left (fun r (na,ps,e,ftag,rtag) -> 
						let patfn = genvar () in
						let epatfn = EVar(patfn),deb in
						(ELetRec([na,ps,
							(EMatch(
								(eapp (evar fsn) [EVariant(ftag,[]),deb]),
								[[PVariantApp(rtag,PVar(patfn))],(eapp epatfn (List.map (pattern2expr deb) ps))]
							),deb)],
						r),deb)
					) e2 napsefrtag
				))
			(*
				let tnas = List.mapi (fun i x -> (genvar ()) ^ (Printf.sprintf "_tf_%d_" i) ^ x) nas in
				let _,add_tf = List.fold_left (fun (fns,rf) (tna,na) -> 
					let etna = EVar(tna),deb in
					let appe = if List.length fns > 0 then EApp(etna,fns),deb else etna in
					let trf = fun e -> ELet(na,appe,rf e),deb in
					(etna :: fns,trf)
				) ([],(fun e -> e)) (List.combine tnas nas) in
				
				let tnapse = List.map2 (fun tna (_,ps,e) -> (tna,ps,e)) tnas napse in
				let basee = add_tf e2 in
				let _,(res,_) = List.fold_left (fun (fns,r) (tna,ps,e) -> 
					PVar(tna) :: fns,(ELetRec([
					tna,fns @ ps,
					add_tf e],r),deb)
				) ([],basee) tnapse 
				in
				res
				*)
			)
		| _ -> ast
	in
		tast,deb

let unfold_match_pattern astdeb = 
	let ast,deb = astdeb in
	let tast = 
		match ast with
		| EMatch(e1,eps) -> (
				EMatch(e1,
					List.fold_right (fun (ps,e) r -> (* これfold_left だと優先度が逆になってしまう *)
						(List.map (fun p -> ([p],e)) ps) @ r
					) eps []
				)
			)
		| _ -> ast
	in
		tast,deb

let rec conv_expr conv astdeb = 
	let ast,deb = (conv astdeb) in
	let self = conv_expr conv in
	let tast = (
		match ast with
		| ELetRec(napse,e2) -> ELetRec(List.map (fun (na,ps,e) -> na,ps,self e) napse,self e2)
		| ELet(na,e2,e3) -> ELet(na,self e2,self e3)
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


let preprocess_expr astd = 
	conv_expr (fun x -> x |> unfold_let_rec_and |> unfold_let_bind |> unfold_match_pattern) astd


let rec preprocess_decl de =
	match de with
	| DLet(na,e) -> (
			let te = preprocess_expr e in
			[FDecl(DLet(na,te))] 
		)
	| DLetRec(navse) -> (
			(*
			ここで例のletに変換するやつをやってしまう
			let te = preprocess_expr e in
			[FDecl(DLetRec(na,vs,te))] 
			*)
			let ls = List.length navse in
			let dd = default_debug_data in
			let tds = 
				if ls = 1 then (
					let na,_,_ = List.hd navse in
						[DLet(na,(ELetRec(navse,(EVar(na),dd)),dd))]
				) else (
					let pnas = genvar () in
					let nas = List.map (fun (x,_,_) -> x) navse in
					[
						DLet(pnas,(ELetRec(navse,(ETuple(List.map (fun x -> (EVar(x),dd)) nas),dd)),dd));
					] @ (
						List.mapi (fun i x -> DLet(x,(EOp(OGetTupleWithLen(ls,i),[EVar(pnas),dd]),dd))) nas
					)
				)
			in
			List.concat (List.map preprocess_decl tds)
		)
	| DTypeRename _ | DVariant _ | DOpen _ -> [FDecl(de)]

let preprocess asts = 
	List.flatten (List.map (fun x -> 
		match x with
		| FExpr e -> [FExpr(preprocess_expr e)]
		| FDecl d -> preprocess_decl d
	) asts)
