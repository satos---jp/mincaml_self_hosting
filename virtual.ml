open Closure_conv
open Syntax

open Type_checker
open Debug
type name = string * (ty * debug_data)

let genlabel = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@label_%d" !c)

type label = string
type op = 
	| OpMovi  of name * const
	| OpMov   of name * name
	| OpOpr   of name * optype * (name list)
	| OpJcnd  of comptype * name * name * label
	| OpLabel of label 
	| OpJmp   of label 
	| OpApp   of name * name * (name list)
	| OpDirApp   of name * name * (name list)
	| OpMakeTuple of name * (name list)
	| OpDestTuple of (name list) * name
	| OpMakeCls   of name * name * (name list)
	| OpRet       of name
	| OpMainRet

(*
型情報について
Movi .. 不要
Mov .. どちらかいる
Opr .. 必要(Oprにのってるきがしたが、ArrReadなどはいる)
Jcnd .. (いまのところ)不要(intの比較なので)
Label .. なし
Jmp .. なし
App .. 必要(引数もしくは関数に)
MakeTuple,DestTuple .. 必要(どっちかに)
MakeDCls .. 必要(どっちかに)
OpRet .. 必要
*)


let rec to_asms ast tov = 
	match ast with
	| CConst(x) -> [OpMovi(tov,x)]
	| COp(x,vs) -> [OpOpr(tov,x,vs)]
	| CLet(na,e1,e2) -> (to_asms e1 na) @ (to_asms e2 tov)
	| CIf(cmpty,a,b,e1,e2) -> (
	(*
		if ty a b in jmp lst
		e1
		jmp lgl
	lst:
		e2
	lgl:
	*)
			let lst = genlabel () in
			let lgl = genlabel () in			
				OpJcnd(cmpty,a,b,lst) :: (to_asms e1 tov) @ 
					[OpJmp(lgl);OpLabel(lst)] @ (to_asms e2 tov) @ [OpLabel(lgl)]
		)
	| CVar(x) -> (
		if List.mem (fst x) global_funcs then 
			to_asms (CClosure(x,[])) tov
		else
			[OpMov(tov,x)]
		)
	| CApp(a,b) -> [OpApp(tov,a,b)]
	| CDirApp(a,b) -> [OpDirApp(tov,a,b)]
	| CTuple(vs) -> [OpMakeTuple(tov,vs)]
	| CLetTuple(vs,ta,e1) -> OpDestTuple(vs,ta) :: (to_asms e1 tov)
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

let collect_names ast vs = remove_vals (unique_name (collect_names_rec ast)) ((List.map fst vs) @ global_funcs)

let names2str vs = "[\n" ^ (String.concat ";\n" (List.map name2str vs)) ^ ";\n]\n"

let rec to_virtual (defs,rd) = 
	(List.map (fun (na,(vs1,vs2,bo)) -> 
		let rd = snd (snd na) in
		let rv = "@ret_val_" ^ (fst na) in
		let rt = 
			match fst (snd na) with
			| TyFun(_,x) -> x
			| x -> raise (Failure ("Type " ^ (type2str x) ^ " is not function type"))
		in
		let rtd = (rt,rd) in
			(na,vs1,vs2,((to_asms bo (rv,rtd)) @ [OpRet((rv,rtd))],(rv,rtd) :: (collect_names bo (vs1 @ vs2))))) defs,
	
	let gvt = ("@global_ret_val",(TyInt,default_debug_data)) in
	((to_asms rd gvt) @ [OpMainRet],gvt :: (collect_names rd [])))







