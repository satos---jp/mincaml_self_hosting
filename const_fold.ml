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
						| Omul -> KOp(Oimul(x),[nad])
						| _ -> raise Not_found
					)
				| [Right(nad);Left(CInt x)] -> (
						match op with
						| Omul -> KOp(Oimul(x),[nad])
						| Odiv -> KOp(Oibydiv(x),[nad])
						| _ -> raise Not_found
					)
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
	| KLet((na,_) as natd,e1,e2) -> (
			let te1 = reccall e1 in
			KLet(natd,te1,(
				match te1 with
				| KTuple vs -> const_fold ((na,vs) :: tupleenv) cenv e2
				| KConst c -> const_fold tupleenv ((na,c) :: cenv) e2
				| _ -> reccall e2))
		)
	| KOp(op,vs) -> op_fold op vs
	| _ -> kexp_recconv reccall ast

let folder ast = const_fold [] [] ast
