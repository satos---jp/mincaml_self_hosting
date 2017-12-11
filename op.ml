open Syntax
open Type_checker
open Debug

type name = string * (ty * debug_data)
let name2str (na,(ty,_)) = na 
let vs2str vs = "(" ^ (String.concat " , " (List.map name2str vs)) ^ ")"

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

