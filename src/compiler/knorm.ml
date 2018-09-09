open Syntax
open Debug
open Genint
open Type_checker

type name = string * (ty * debug_data)

type kexp = 
	| KConst of Syntax.const
	| KOp        of Syntax.optype * (name list)
	| KIf        of comptype * name * name * kexp * kexp
	| KLet       of name * kexp * kexp
	| KVar       of name
	| KLetRec    of name * (name list) * kexp * kexp
	| KApp       of name * (name list)
	| KTuple     of (name list)
	| KLetTuple  of (name list) * name * kexp


(* デバッグ情報はいったん落とす(実装がつらい...) *) 

type hash = Vs of int * (string list) | C of const | N of hash list 


let rec hasher ast = 
	match ast with
	| KConst c -> C c
	| KOp(op,vs) -> Vs(1,(op2str op) :: (List.map fst vs))
	| KIf(op,a,b,e1,e2) -> N([Vs(2,(comptype2str op) :: (List.map fst [a;b]));hasher e1;hasher e2])
	| KLet((na,_),e1,e2) -> N([Vs(3,[na]);hasher e1; hasher e2])
	| KVar((na,_)) -> Vs(4,[na])
	| KLetRec((fn,_),vs,e1,e2) -> N([Vs(5,fn :: (List.map fst vs));hasher e1;hasher e2])
	| KApp((na,_),vs) -> Vs(6,na :: (List.map fst vs))
	| KTuple(vs) -> Vs(7,List.map fst vs)
	| KLetTuple(vs,(na,_),e1) -> N([Vs(8,na :: (List.map fst vs));hasher e1])


(* よくある再帰的なモナド的な *)
let rec kexp_recconv reccall ast = 
	match ast with
	| KIf(c,a,b,e1,e2) -> KIf(c,a,b,reccall e1,reccall e2)
	| KLetRec(fn,vs,e1,e2) -> KLetRec(fn,vs,reccall e1,reccall e2)
	| KLetTuple(a,b,e1) -> KLetTuple(a,b,reccall e1)
	| KLet(na,e1,e2) -> KLet(na,reccall e1,reccall e2)
	| _ -> ast


let rec kexp_recconv_withrename reccall env ast = 
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
	| KIf(op,a,b,e1,e2) -> KIf(op,conv_var a,conv_var b,reccall e1,reccall e2)
	| KLet(na,e1,e2) -> KLet(na,reccall e1,reccall e2)
	| KVar(x) -> KVar(conv_var x)
	| KLetRec(na,vs,e1,e2) -> KLetRec(na,vs,reccall e1,reccall e2)
	| KApp(fn,vs) -> KApp(conv_var fn,conv_list vs)
	| KTuple(vs) -> KTuple(conv_list vs)
	| KLetTuple(vs,na,e1) -> KLetTuple(vs,conv_var na,reccall e1)



let name2str (na,(ty,_)) = na ^ " : " ^ (type2str ty)

let vs2str vs = "(" ^ (String.concat " , " (List.map name2str vs)) ^ ")"

let rec kexp2str_base ast d = 
	match ast with
	| KConst x -> [(d,const2str x)]
	| KOp(op,vs) -> [(d,(op2str op) ^ (vs2str vs))]
	| KLet(na,e1,e2) -> (d,"Let " ^ (name2str na) ^ " =") :: (kexp2str_base e1 (d+1)) @ [(d,"In")] @ (kexp2str_base e2 (d+1))
	| KLetRec(fn,vs,e1,e2) -> (d,"Let Rec " ^ (name2str fn) ^ " " ^(vs2str vs) ^ " =") :: (kexp2str_base e1 (d+1)) @ [(d,"In")] @ (kexp2str_base e2 d)
	| KIf(ty,a,b,e1,e2) -> (d,"If " ^ (name2str a) ^ " " ^ (comptype2str ty) ^ " " ^ (name2str b) ^ " Then") :: (kexp2str_base e1 (d+1)) @ [(d,"Else")] @ (kexp2str_base e2 (d+1))
	| KVar(x) -> [(d,"Var " ^ (name2str x))]
	| KApp(fn,vs) -> [(d,"App " ^ (name2str fn) ^ (vs2str vs))]
	| KTuple(vs) -> [(d,(vs2str vs))]
	| KLetTuple(vs,tn,e1) -> (d,"Let " ^ (vs2str vs) ^ " = " ^ (name2str tn)) :: [(d,"In")] @ (kexp2str_base e1 (d+1))

let knorm2str ast = 
	let ss = kexp2str_base ast 0 in
		(String.concat "\n" (List.map (fun (d,s) -> (String.make (d*2) ' ') ^ s) ss)) ^ "\n"

let rec kexp_size ast = 
	match ast with
	| KIf(_,_,_,e1,e2) | KLet(_,e1,e2) | KLetRec(_,_,e1,e2) -> (
			1 + (kexp_size e1) + (kexp_size e2)
		)
	| KLetTuple(_,_,e1) -> 1 + (kexp_size e1)
	| _ -> 1

let genvar () = Printf.sprintf "@k_%d" (genint ())

let rec knorm (ast,nt) = 
	match ast with
	| TConst x -> KConst x
	| TVar x -> KVar x
	| TOp(s,xs) -> (
			let vxs = List.map (fun e -> (e,genvar ())) xs in
			List.fold_right (fun ((ne,net),nv) -> fun r -> KLet((nv,net),knorm (ne,net),r))
			vxs (KOp(s,List.map (fun ((_,t),v) -> (v,t)) vxs)) 
		)
	(* matchで生じる true,falseの畳み込み(だいぶアドホックだが)
	| TIf((TConst(CInt(v)),_),e3,e4)  -> (
			if v == 0 then knorm e4 else knorm e3
		)
	*)
	| TIf((TOp(op,[e1;e2]),opt),e3,e4) 
		when List.mem op [Oeq; Oneq; Olt; Oleq; Ogt; Ogeq] -> (
			let vt1 = (genvar ()),(snd e1) in
			let vt2 = (genvar ()),(snd e2) in
			let k3 = knorm e3 in
			let k4 = knorm e4 in
			KLet(vt1,knorm e1,
				KLet(vt2,knorm e2,
					(match op with
					| Oeq  -> KIf(CmpEq,vt1,vt2,k3,k4)
					| Oneq -> KIf(CmpEq,vt1,vt2,k4,k3)
					
					| Ogeq -> KIf(CmpLt,vt1,vt2,k3,k4)
					| Oleq -> KIf(CmpLt,vt2,vt1,k3,k4)
					| Ogt -> KIf(CmpLt,vt2,vt1,k4,k3)
					| Olt -> KIf(CmpLt,vt1,vt2,k4,k3)
					| _ -> raise (Failure "shouldn't reach here")
					)
				)
			)
		)
	| TIf(e1,e2,e3) -> (
			let (_,(_,d)) = e1 in
			let tast = TIf(
				(TOp(Oeq,[e1;(TConst(CInt 1),(TyInt,d))]),(TyInt,d))
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

