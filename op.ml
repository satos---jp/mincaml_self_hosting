open Syntax
open Type_checker
open Debug

type namestr = Var of string | Reg of string
type namereg = (namestr ref) * (ty * debug_data)

type name = string * (ty * debug_data)
let name2str (na,(ty,_)) = na 

let namereg2str (na,(ty,_)) = 
	match !na with
	| Var x -> "Var_" ^ x 
	| Reg x -> "Reg_" ^ x 

let vs2str vs = "(" ^ (String.concat " , " (List.map namereg2str vs)) ^ ")"

let cna2na (na,td) = (ref (Var na),td) 
let cvs2vs vs = List.map cna2na vs


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


let get_var_names op = 
	let res = 
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
	in
		List.fold_left (fun r -> fun x -> match !(fst x) with Var a -> (a,snd x) :: r | _ -> r) [] res

let get_assigned op = 
	let vs = 
	(match op with
	| OpMovi(na,_) | OpMov(na,_) | OpOpr(na,_,_) | OpApp(_,_,na,_,_) | OpMakeTuple(na,_) | OpMakeCls(na,_,_) -> [na]
	| OpJcnd _ | OpJmp _ | OpLabel _ | OpRet _ | OpMainRet -> []
	| OpDestTuple(vs,_) -> vs
	) in
	List.map (fun (x,_) -> !x) vs
	
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

