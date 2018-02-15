open Type_checker
open Op

let rec range a b =  
	if a >= b then [] else a :: (range (a+1) b)

type register_convention = {
	ty2savereg : ty -> string list;
	ty2argreg  : ty -> string list;
	ty2retreg : ty -> string;
	savereg : string list;
	
	args2regs : 'a.
	(namereg list) ->
	(string -> namereg -> 'a) -> 
	(string -> namereg -> 'a) -> 	
	(int -> namereg -> 'a) -> 
	('a list)
}

let rreg = List.map (fun x -> Printf.sprintf "r%d" x) ([8]  @ (range 20 31))
let freg = List.map (fun x -> Printf.sprintf "f%d" x) ([4] @ (range 6 10) @ (range 20 32)) 
let func_rreg = List.map (fun x -> Printf.sprintf "r%d" x) (range 10 20) 
let func_freg = List.map (fun x -> Printf.sprintf "f%d" x) (range 10 20) 


let tortesia_register_convention = 
	{
		ty2savereg = (fun t -> match t with TyFloat -> freg | _ -> rreg);
		ty2argreg = (fun t -> match t with TyFloat -> func_freg | _ -> func_rreg);
		ty2retreg = (fun t -> match t with TyFloat -> "f5" | _ -> "r9");
		savereg = rreg @ freg;
		
		(* 引数のnamereg配列をレジスタ名にしていく *)
		args2regs = (fun vs rf ff otherfunc ->
			let rec recf nas i nrs nfs = 
				match nas with
				| [] -> []
				| ((_,(t,_)) as x) :: xs -> (
					match t,nrs,nfs with
					| TyFloat,_,f :: fs -> (ff f x) :: (recf xs (i+1) nrs fs)
					| TyFloat,_,[] -> (otherfunc i x) :: (recf xs (i+1) nrs nfs)
					| _,r :: rs,_ -> (rf r x) :: (recf xs (i+1) rs nfs)
					| _ ,[],_-> (otherfunc i x) :: (recf xs (i+1) nrs nfs)
				)
			in
				recf vs 0 func_rreg func_freg
		);
}


