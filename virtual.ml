open Closure_conv
open Syntax


let genlabel = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@label_%d" !c)

type label = string
type op = 
	| OpMovi  of name * const
	| OpMov   of name * name
	| OpOpr   of name * optype * (name list)
	| OpJcnd  of comptype * name * name * label
	| OpLabel of label 
	| OpJmp   of label 
	| OpApp   of name * (name list) * name
	| OpMakeTuple of (name list) * name
	| OpDestTuple of name * (name list)
	| OpMakeCls   of name * (name list) * name


let rec to_asms ast tov = 
	match ast with
	| CConst(x) -> [OpMovi(tov,x)]
	| COp(x,vs) -> [OpOpr(tov,x,vs)]
	| CLet(na,e1,e2) -> (to_asms e1 na) @ (to_asms e2 tov)
	| CIf(ty,a,b,e1,e2) -> (
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
				OpJcnd(ty,a,b,lst) :: (to_asms e1 tov) @ 
					[OpJmp(lgl);OpLabel(lst)] @ (to_asms e2 tov) @ [OpLabel(lgl)]
		)
	| CVar(x) -> [OpMov(tov,x)]
	| CApp(a,b) -> [OpApp(a,b,tov)]
	| CTuple(vs) -> [OpMakeTuple(vs,tov)]
	| CLetTuple(vs,ta,e1) -> OpDestTuple(ta,vs) :: (to_asms e1 tov)
	| CClosure(na,vs) -> [OpMakeCls(na,vs,tov)]


(*
let rec to_virtual (rd,defs) = 
	(to_asms 
	List.map (fun (na,x) -> 
		match x with
*)






