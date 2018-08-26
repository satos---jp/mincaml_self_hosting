open Knorm
open Syntax


type ('a,'b) either = Left of 'a | Right of 'b

let rec const_fold tupleenv cenv ast = 
	let reccall = const_fold tupleenv cenv in
	let op_fold op vs = 
		try
			let tvs = List.map (fun ((na,_) as nad) -> try Left(List.assoc na cenv) with Not_found -> Right(nad) ) vs in (
				match tvs with
				| [Left(CInt x)] -> KConst(CInt(
					(match op with
					| Ominus -> (fun x -> -x)
					| Onot -> (fun x -> if x = 0 then 1 else 0)
					| _ -> raise Not_found) x))
				| [Left(CInt x);Left(CInt y)] -> KConst(CInt(
					(match op with
					| Oadd -> (+)
					| Osub -> (-)
					| Omul -> ( * )
					| Odiv -> (/)
					| Oeq -> (fun x y -> if x = y then 1 else 0)
					| Oneq -> (fun x y -> if x <> y then 1 else 0)
					| Olt -> (fun x y -> if x < y then 1 else 0)
					| Ogt -> (fun x y -> if x > y then 1 else 0)
					| Oleq -> (fun x y -> if x <= y then 1 else 0)
					| Ogeq -> (fun x y -> if x >= y then 1 else 0)
					| _ -> raise Not_found) x y))
				| [Left(CInt x);Right(nad)] -> (
						match op with
						| Oadd -> KOp(Oiadd(x),[nad])
						| Omul -> KOp(Oimul(x),[nad])
						| _ -> raise Not_found
					)
				| [Right(nad);Left(CInt x)] -> (
						match op with
						| Oadd -> KOp(Oiadd(x),[nad])
						| Omul -> KOp(Oimul(x),[nad])
						| Osub -> KOp(Oibysub(x),[nad])
						| Odiv -> KOp(Oibydiv(x),[nad])
						| OArrRead -> KOp(OiArrRead(x),[nad])
						| _ -> raise Not_found
					)
				(* 定数配列読み書きを入れて、51億命令 -> 38億命令になった *)
				| [Right(nad);Left(CInt x);Right(ncd)] -> (
						match op with
						| OArrWrite -> KOp(OiArrWrite(x),[nad;ncd])
						| _ -> raise Not_found
					)
				(*
				| [Right(nad);Left(CInt x);Left(ncd)] -> (
						match op with
						| OArrWrite -> KOp(OiArrWrite(x),[nad;ncd])
						| _ -> raise Not_found
					)
				*)
				| [Left(CFloat x);Left(CFloat y)] -> KConst(CFloat(
					(match op with
					| Ofadd -> (+.)
					| Ofsub -> (-.)
					| Ofmul -> ( *.)
					| Ofdiv -> (/.)
					| _ -> raise Not_found) x y))
				| _ -> raise Not_found
			)
		with | Not_found -> KOp(op,vs)
	in
	match ast with
	| KOp(OGetTuple(i),[(na,_)]) -> (
			try 
				let vs = List.assoc na tupleenv in
					KVar(List.nth vs i)
			with
				| Not_found -> ast
		)
	(* 末尾再帰のため、ここで消しておく。 *)
	| KOp(Osemi1,[_]) -> (
			KTuple([])
		)
	| KOp(Osemi2,[_;nb]) -> (
			KVar(nb)
		)
	| KOp(op,vs) -> op_fold op vs
	| KLet((na,_) as natd,e1,e2) -> (
			let te1 = reccall e1 in
			KLet(natd,te1,(
				match te1 with
				| KTuple vs -> const_fold ((na,vs) :: tupleenv) cenv e2
				| KConst c -> const_fold tupleenv ((na,c) :: cenv) e2
				| _ -> reccall e2))
		)
	(* matchで生じる true,falseの畳み込み(だいぶアドホックだが)*)
	(* TODO CmpLt とかも畳み込む *)
	| KIf(CmpEq,((na,_) as nda),((nb,_) as ndb),e3,e4)  -> (
			let te3 = reccall e3 in
			let te4 = reccall e4 in
			try 
				let v = List.assoc na cenv in
				let w = List.assoc nb cenv in
				(*
				Printf.printf "folding %s := %s Cmpeq %s := %s with %b\n" na (const2str v) nb (const2str w) (v == w);
				*)
				match v,w with
				| CInt(x),CInt(y) -> if x == y then te3 else te4
				| _ -> KIf(CmpEq,nda,ndb,te3,te4)
			with
				| Not_found -> (
						(*
						Printf.sprintf "not found %s or %s" na nb;
						*)
						KIf(CmpEq,nda,ndb,te3,te4)
					)
		)
	| _ -> kexp_recconv reccall ast

let folder ast =
	const_fold [] [] ast

