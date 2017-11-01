open Syntax
open Knorm
open Type_checker

(* 
・関数の引数のTupleを分解する 
・LetTupleを分解する(これは、縮むならConstFoldに投げる)

この変換は、たぶん、最初の数回だけでよさそう
*)

let genname = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@let_dest_%d" !c)

let type_flatten t = 
	match t with
	| TyFun(ts,tt) -> (
			let tts = List.fold_right (fun t -> fun r -> 
				match t with
				| TyTuple(tts) -> tts @ r
				| _ -> t :: r
			) ts []
			in TyFun(tts,tt)
		)
	| _ -> raise (Failure (Printf.sprintf "type %s is not function type" (type2str t)))

let rec remove_tuple ast = 
	let reccall = remove_tuple in
	match ast with
	| KLetRec((fn,(ft,fd)),vs,e1,e2) -> (
			let tft = type_flatten ft in
			let tvs,te1 = List.fold_right (fun ((na,(nt,nd)) as natd) -> fun (rvs,re1) -> 
				match nt with
				| TyTuple ts -> (
						let tvs = List.map (fun t -> (genname (),(t,nd))) ts in
						(tvs @ rvs),KLet(natd,KTuple(tvs),re1)
					)
				| _ -> (natd :: rvs),re1
			) vs ([],e1) in
			KLetRec((fn,(tft,fd)),tvs,remove_tuple te1,remove_tuple e2)
		)
	| KLetTuple(vs,tn,e1) -> (
			let te1,_ = List.fold_left (fun (r,i) -> fun na -> 
				(KLet(na,KOp(OGetTuple(i),[tn]),r),i+1)
			) (remove_tuple e1,0) vs in te1
		)
	| KApp((fn,(ft,fd)),vs) -> (
			let tvs,tfs = List.fold_right (fun ((na,(nt,nd)) as natd) -> fun (rvs,rf) -> 
				match nt with
				| TyTuple ts -> (
						let tvs = List.map (fun t -> (genname (),(t,nd))) ts in
						(tvs @ rvs),(fun x -> reccall (KLetTuple(tvs,natd,x)))
					)
				| _ -> (natd :: rvs,rf)
			) vs ([],fun x -> x)
			in
				tfs (KApp((fn,(type_flatten ft,fd)),tvs))
		)
	| _ -> kexp_recconv reccall ast

let lettupledest ast = 
	let nast = ref ast in
	let rec f i = 
		nast := remove_tuple (!nast);
		(* 
		Printf.printf "step dest %d\n" (kexp_size !nast);  
		Printf.printf "%s" (knorm2str !nast); 
		print_newline ();
		無限ループするのでもうだめです *)
		if (hasher !nast) = (hasher ast) || i > 100 then !nast else f (i+1)
	in f 0


