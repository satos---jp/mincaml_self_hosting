open Closure_conv
open Syntax
open Type_checker
open Debug
open Genint
open Op
open Cfg
open Main_option

(*
let name2str (na,(ty,_)) = na ^ " : " ^ (type2str ty)
量がやばい
*)


let genlabel () = Printf.sprintf "@virtual_label_%d" (genint ())



let rec to_asms ast tov istail retop = 
	match ast with
	| CConst(x) -> [OpMovi(tov,x)]
	| COp(x,vs) -> [OpOpr(tov,x,vs)]
	| CLet(na,e1,e2) -> (to_asms e1 na false retop) @ (to_asms e2 tov istail retop)
	| CIf(cmpty,a,b,e1,e2) -> (
	(*
		if ty a b then jmp lst
		e1
		jmp lgl
	lst:
		e2
	lgl:
	*)
			let lst = genlabel () in
			let lgl = genlabel () in			
				OpJcnd(cmpty,a,b,lst) :: (to_asms e1 tov istail retop) @ 
					(if istail then retop else [OpJmp(lgl)]) @ [OpLabel(lst)] @ (to_asms e2 tov istail retop) @ [OpLabel(lgl)]
		)
	| CVar(x) -> (
		if List.mem (fst x) (global_funcs ()) then 
			to_asms (CClosure(x,[])) tov istail retop
		else
			[OpMov(tov,x)]
		)
	| CApp(a,b) -> [OpApp((if istail then Tail else NonTail),InDirApp,tov,a,b)]
	| CDirApp(a,b) -> [OpApp((if istail then Tail else NonTail),DirApp,tov,a,b)]
	| CTuple(vs) -> [OpMakeTuple(tov,vs)]
	| CLetTuple(vs,ta,e1) -> OpDestTuple(vs,ta) :: (to_asms e1 tov istail retop)
	| CClosure(na,vs) -> [OpMakeCls(tov,na,vs)]


let rec collect_names_rec ast = 
	match ast with
	| CConst _ -> []
	| COp(_,vs) -> vs
	| CLet(na,e1,e2) -> na :: (collect_names_rec e1) @ (collect_names_rec e2)
	| CIf(cmpty,a,b,e1,e2) -> a :: b :: (collect_names_rec e1) @ (collect_names_rec e2)
	| CVar(x) -> [x]
	| CApp(a,b) -> a :: b
	| CDirApp(a,b) -> b (* aはglobalにあるやつなので、変数ではない(ハズ) *)
	| CTuple(vs) -> vs
	| CLetTuple(vs,ta,e1) -> ta :: vs @ (collect_names_rec e1) 
	| CClosure(_,vs) -> vs (* closureの名前はglobalにあるやつなので、変数ではない(ハズ) *)

let rec unique_name vs = 
	match vs with
	| [] -> []
	| (x,t) :: xs -> 
		try let _ = List.assoc x xs in unique_name xs
		with
			| Not_found -> (x,t) :: (unique_name xs)

let remove_vals vs ws = List.filter (fun (x,_) -> not (List.mem x ws)) vs 

let collect_names ast vs globvars = remove_vals (unique_name (collect_names_rec ast)) (globvars @ (List.map fst vs) @ (global_funcs ()))

let names2str vs = "[\n" ^ (String.concat ";\n" (List.map name2str vs)) ^ ";\n]\n"



type funbody = {
	ops: op list;
	vs: name list;
}

type virtglobdef = {
	fn: name;
	vs: name list;
	cvs: name list;
	body: funbody;
}

let funbody2str {ops = ops; vs=vs} =
	"localval:: " ^ (String.concat " " (List.map name2str vs)) ^ "\n" ^ 
	(String.concat "\n" (List.map virtop2str ops)) 


let virtglobdef2str {fn=fn; vs=vs; cvs=cvs; body=bo} = 
	(name2str fn) ^ (vs2str vs) ^ (vs2str cvs) ^ ":\n" ^
	(funbody2str bo)

let virt2str (vgs,mfb,gvs) = 
	(String.concat "" (List.map virtglobdef2str vgs)) ^
	"globalval:: " ^ (String.concat " " gvs) ^ "\n" ^ 
	(virtglobdef2str {fn=("main",(TyInt,default_debug_data)); vs=[]; cvs=[]; body=mfb})

let rec to_virtual (fundefs,globvars,rd) = 
	(List.map (fun (na,def) -> 
		match def with
		| ClosFunDef(vs1,vs2,bo) -> (
			let rd = snd (snd na) in
			let rv = "@ret_val_" ^ (fst na) in
			let rt = 
				match fst (snd na) with
				| TyFun(_,x) -> x
				| x -> raise (Failure ("Type " ^ (type2str x) ^ " is not function type"))
			in
			let rtd = (rt,rd) in
			let retop = [OpRet((rv,rtd))] in
			let args = vs1 @ vs2 in
				{fn = na; vs = vs1; cvs = vs2; body = {
					ops = if !tortesia then (cfg_toasms bo (rv,rtd) true retop args na) @ retop else (to_asms bo (rv,rtd) true retop) @ retop ; 
					vs = (rv,rtd) :: (collect_names bo args globvars)
				};})
		) fundefs),
	(let gvt = ("@global_ret_val",(TyInt,default_debug_data)) in
	let gfn = ("@global_main_func",(TyInt,default_debug_data)) in
	let retop = [OpMainRet] in
	{
		ops = if !tortesia then (cfg_toasms rd gvt false retop [] gfn) @ retop else (to_asms rd gvt false retop) @ retop; 
		vs = gvt :: (collect_names rd [] globvars);
	}),
	(* mainで末尾再帰はしなくてよいはず。 *)
	globvars





