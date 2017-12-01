open Closure_conv
open Syntax
open Type_checker
open Debug
open Genint

type name = string * (ty * debug_data)
(*
let name2str (na,(ty,_)) = na ^ " : " ^ (type2str ty)
量がやばい
*)
let name2str (na,(ty,_)) = na 


let vs2str vs = "(" ^ (String.concat " , " (List.map name2str vs)) ^ ")"

let genlabel () = Printf.sprintf "@virtual_label_%d" (genint ())

type label = string
type istailcall = Tail | NonTail
type isdirapp   = DirApp | InDirApp
type op = 
	| OpMovi  of name * const
	| OpMov   of name * name
	| OpOpr   of name * optype * (name list)
	| OpJcnd  of comptype * name * name * label
	| OpLabel of label 
	| OpJmp   of label 
	| OpApp   of istailcall * isdirapp * name * name * (name list)
	| OpMakeTuple of name * (name list)
	| OpDestTuple of (name list) * name
	| OpMakeCls   of name * name * (name list)
	| OpRet       of name
	| OpMainRet

let rec virtop2str op = 
	match op with
	| OpMovi(na,c) -> "\t" ^ (name2str na) ^ " := " ^ (const2str c)
	| OpMov(na,c) -> "\t" ^ (name2str na) ^ " := " ^ (name2str na)
	| OpOpr(na,opr,vs) -> "\t" ^ (name2str na) ^ " := " ^ (op2str opr) ^ (vs2str vs)
	| OpJcnd(cmp,na,nb,la) -> "\tIf " ^ (name2str na) ^ " " ^ (comptype2str cmp) ^ " " ^ (name2str nb) ^ " Then Jmp " ^ la
	| OpJmp(la) -> "\tJmp " ^ la
	| OpLabel(la) -> la ^ ":"
	| OpApp(b1,b2,na,nb,vs) -> "\t" ^ (name2str na) ^ " := " ^ (name2str nb) ^ (vs2str vs)
	| OpMakeTuple(na,vs) -> "\t" ^ (name2str na) ^ " := " ^ (vs2str vs)
	| OpDestTuple(vs,na) -> "\t" ^ (vs2str vs) ^ " := " ^ (name2str na) 
	| OpMakeCls(na,nb,vs) -> "\t" ^ (name2str na) ^ " := <Closure: " ^ (name2str nb) ^ (vs2str vs) ^ " >"
	| OpRet(na) -> "\tReturn " ^ (name2str na)
	| OpMainRet -> "\tMainReturn " 



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


let rec to_asms ast tov istail retop = 
	match ast with
	| CConst(x) -> [OpMovi(tov,x)]
	| COp(x,vs) -> [OpOpr(tov,x,vs)]
	| CLet(na,e1,e2) -> (to_asms e1 na false retop) @ (to_asms e2 tov istail retop)
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

type funbody = VirtFunBody of op list * name list
type virtglobdef = VirtFunDef of name * (name list) * (name list) * funbody

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
				VirtFunDef(na,vs1,vs2,VirtFunBody((to_asms bo (rv,rtd) true retop) @ retop,(rv,rtd) :: (collect_names bo (vs1 @ vs2) globvars))))
		) fundefs),
	
	(let gvt = ("@global_ret_val",(TyInt,default_debug_data)) in
	let retop = [OpMainRet] in
	VirtFunBody((to_asms rd gvt false retop) @ retop,gvt :: (collect_names rd [] globvars))),
	(* mainで末尾再帰はしなくてよいはず。 *)
	globvars


let funbody2str fb =
	match fb with
	| VirtFunBody(ops,vs) -> (
			"localval:: " ^ (String.concat " " (List.map name2str vs)) ^ "\n" ^ 
			(String.concat "\n" (List.map virtop2str ops)) 
		)

let virtglobdef2str gdf = 
	match gdf with
	| VirtFunDef(fn,vs,cvs,bo) -> (
			(name2str fn) ^ (vs2str vs) ^ (vs2str cvs) ^ ":\n" ^
			(funbody2str bo)
		)

let virt2str (vgs,mfb,gvs) = 
	(String.concat "" (List.map virtglobdef2str vgs)) ^
	"globalval:: " ^ (String.concat " " gvs) ^ "\n" ^ 
	(virtglobdef2str (VirtFunDef(("main",(TyInt,default_debug_data)),[],[],mfb)))





