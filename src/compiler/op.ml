open Syntax
open Type_checker
open Debug

type namestr = GVar of string | Var of string | Reg of string
type namereg = (namestr ref) * (ty * debug_data)

type name = string * (ty * debug_data)
let name2str (na,(ty,_)) = na 

let namestr2str na = 
	match na with
	| GVar x -> "GlobalVar_" ^ x 
	| Var x -> "Var_" ^ x 
	| Reg x -> "Reg_" ^ x 
	
let namereg2str (na,(ty,_)) = namestr2str !na

let vs2str vs = "(" ^ (String.concat " , " (List.map namereg2str vs)) ^ ")"

type label = string
type istailcall = Tail | NonTail
type isdirapp   = DirApp | InDirApp
type op = 
	| OpMovi  of namereg * const
	| OpMov   of namereg * namereg
	| OpOpr   of namereg * optype * (namereg list)
	| OpJcnd  of comptype * namereg * namereg * label
	| OpLabel of label 
	| OpJmp   of label 
	| OpApp   of istailcall * isdirapp * namereg * namereg * (namereg list)
	| OpMakeTuple of namereg * (namereg list)
	| OpDestTuple of (namereg list) * namereg
	| OpMakeCls   of namereg * name * (namereg list)
	| OpRet       of namereg
	| OpMainRet

let virtop2str op = 
	match op with
	| OpMovi(na,c) -> "\t" ^ (namereg2str na) ^ " := " ^ (const2str c)
	| OpMov(na,nb) -> "\t" ^ (namereg2str na) ^ " := " ^ (namereg2str nb)
	| OpOpr(na,opr,vs) -> "\t" ^ (namereg2str na) ^ " := " ^ (op2str opr) ^ (vs2str vs)
	| OpJcnd(cmp,na,nb,la) -> "\tIf " ^ (namereg2str na) ^ " " ^ (comptype2str cmp) ^ " " ^ (namereg2str nb) ^ " Then Jmp " ^ la
	| OpJmp(la) -> "\tJmp " ^ la
	| OpLabel(la) -> la ^ ":"
	| OpApp(b1,b2,na,nb,vs) -> "\t" ^ (namereg2str na) ^ " := " ^ (namereg2str nb) ^ (vs2str vs)
	| OpMakeTuple(na,vs) -> "\t" ^ (namereg2str na) ^ " := " ^ (vs2str vs)
	| OpDestTuple(vs,na) -> "\t" ^ (vs2str vs) ^ " := " ^ (namereg2str na) 
	| OpMakeCls(na,nb,vs) -> "\t" ^ (namereg2str na) ^ " := <Closure: " ^ (name2str nb) ^ (vs2str vs) ^ " >"
	| OpRet(na) -> "\tReturn " ^ (namereg2str na)
	| OpMainRet -> "\tMainReturn " 


let get_var_nameregs op = 
	match op with
	| OpMovi(na,c) -> [na]
	| OpMov(na,nb) -> [na;nb]
	| OpOpr(na,opr,vs) -> na :: vs
	| OpJcnd(cmp,na,nb,la) -> [na;nb]
	| OpJmp(la) -> []
	| OpLabel(la) -> []
	| OpApp(b1,b2,na,nb,vs) -> na :: nb :: vs 
	| OpMakeTuple(na,vs) -> na :: vs
	| OpDestTuple(vs,na) -> na :: vs
	| OpMakeCls(na,nb,vs) -> na :: vs (* globalな関数名は含まない *)
	| OpRet(na) -> [na]
	| OpMainRet -> []

let get_assigned op = 
	let vs = 
	(match op with
	| OpMovi(na,_) | OpMov(na,_) | OpOpr(na,_,_) | OpApp(_,_,na,_,_) | OpMakeTuple(na,_) | OpMakeCls(na,_,_) -> [na]
	| OpJcnd _ | OpJmp _ | OpLabel _ | OpRet _ | OpMainRet -> []
	| OpDestTuple(vs,_) -> vs
	) in
	List.map (fun (x,_) -> !x) vs

let get_assigner op = 
	let vs = 
	(match op with
	| OpMovi(na,c) -> []
	| OpMov(na,nb) -> [nb]
	| OpOpr(na,opr,vs) -> vs
	| OpJcnd(cmp,na,nb,la) -> [na;nb]
	| OpJmp(la) -> []
	| OpLabel(la) -> []
	| OpApp(b1,b2,na,nb,vs) -> nb :: vs 
	| OpMakeTuple(na,vs) -> vs
	| OpDestTuple(vs,na) -> [na]
	| OpMakeCls(na,nb,vs) -> vs (* globalな関数名は含まない *)
	| OpRet(na) -> [na]
	| OpMainRet -> []
	) in
	List.map (fun (x,_) -> x) vs
	

let subst_assigner op f = 
	match op with
	| OpMovi _ | OpJmp _ | OpLabel _ | OpMainRet -> op
	| OpMov(na,nb) -> OpMov(na,f nb)
	| OpOpr(na,opr,vs) -> OpOpr(na,opr,List.map f vs)
	| OpJcnd(cmp,na,nb,la) -> OpJcnd(cmp,f na,f nb,la)
	| OpApp(b1,b2,na,nb,vs) -> OpApp(b1,b2,na,f nb,List.map f vs)
	| OpMakeTuple(na,vs) -> OpMakeTuple(na,List.map f vs)
	| OpDestTuple(vs,na) -> OpDestTuple(vs,f na)
	| OpMakeCls(na,nb,vs) -> OpMakeCls(na,nb,List.map f vs) (* globalな関数名は含まない *)
	| OpRet(na) -> OpRet(f na)



let remove_useless_jump ops = 
	(* 連続しているJmpは、最初以外を無視してよい。 *)
 	(* Jmp直後の連続しているラベルのうちに、Jmp先のラベルがあると、Jmpが消える。 *)
 	
 	(* Some(la) labels は、直前が Jmp la; labels である状況。*)
 	
	let rec f lastjmp labels ops = 
		match lastjmp,labels,ops with
		| None,_,[] -> labels
		| Some la,_,[] -> OpJmp(la) :: labels
		
		(* 連続しているJmpは、最初以外を無視してよい。 *)
		| None,[],OpJmp(la) :: xs | Some la,[],OpJmp(_) :: xs -> f (Some(la)) [] xs
		
		(* Jmp直後の連続しているラベルのうちに、Jmp先のラベルがあると、Jmpが消える。 *)
		| Some la,_,OpLabel(lb) :: xs -> (
				if la = lb then f None labels xs
				else f (Some(la)) (OpLabel(lb) :: labels) xs
			)
		| None,_,x :: xs -> labels @ [x] @ (f None [] xs)
		| Some la,_,x :: xs -> OpJmp(la) :: labels @ [x] @ (f None [] xs)
	in
		f None [] ops



