open Genint
open Syntax
open Debug
open Type

let genvar () = Printf.sprintf "@tc_%d" (genint ())

let name2str (na,ty) = na ^ " : " ^ (type2str ty)

type name = string * (ty * debug_data)

type texp = texp_base * (ty * debug_data)
and texp_base =
  | TConst of const
  | TVar       of name
  | TOp        of optype * (texp list)
  | TIf        of texp * texp * texp
  | TLet       of name * texp * texp
  | TLetRec    of name * (name list) * texp * texp
  | TApp       of texp * (texp list)
  | TTuple     of (texp list)
  | TLetTuple  of (name list) * texp * texp

let tdname2str (na,(ty,d)) = na ^ " : " ^ (type2str ty) ^ " : " ^ (debug_data2str d)

let rec texp2str_base astdeb d = 
	let ast,_ = astdeb in
	match ast with
	| TConst x -> [(d,const2str x)]
	| TVar(x) -> [(d,"Var " ^ (tdname2str x))]
	| TOp(op,es) -> (d,op2str op) :: List.concat (List.map (fun x -> (texp2str_base x (d+1))) es)
	| TIf(e1,e2,e3) -> (
			(d,"If") :: (texp2str_base e1 (d+1))
				@ [(d,"Then")] @ (texp2str_base e2 (d+1)) @ [(d,"Else")] @ (texp2str_base e3 (d+1))
		)
	| TLet(na,e1,e2) -> (d,"Let " ^ (tdname2str na) ^ " =") :: (texp2str_base e1 (d+1)) @ [(d,"In")] @ (texp2str_base e2 d)
	| TLetRec(na,vs,e1,e2) -> (d,"LetRec " ^ (tdname2str na) ^ " || " ^ (String.concat " | " (List.map tdname2str vs)) ^  " =") :: (texp2str_base e1 (d+1)) @ [(d,"In")] @ (texp2str_base e2 d)
	| TApp(e1,es) -> (d,"App ") :: (texp2str_base e1 (d+1)) @ List.concat (List.map (fun x -> (texp2str_base x (d+2))) es)
	| TTuple(es) -> (d,"( ") :: List.concat (List.map (fun x -> (texp2str_base x (d+1))) es) @ [(d," )")]
	| TLetTuple(vs,e1,e2) -> (d,"Let " ^ (String.concat " , " (List.map tdname2str vs)) ^ " = ") :: (texp2str_base e1 (d+1)) @ [(d,"In")] @ (texp2str_base e2 d)

let texp2str ast = 
	let ss = texp2str_base ast 0 in
		String.concat "\n" (List.map (fun (d,s) -> (String.make (d*2) ' ') ^ s) ss) ^ "\n"



let rec ast_subst subs (ast,(nt,deb)) = 
	let f = ast_subst subs in
	let mf = List.map f in
	let nf (x,(t,d)) = (x,(ty_subst subs t,d)) in
	let mnf = List.map nf in
	let tast = match ast with
	| TConst _ -> ast
	| TVar(na) -> TVar(nf na)
	| TOp(op,es) -> TOp(op,mf es)
	| TIf(e1,e2,e3) -> TIf(f e1,f e2,f e3)
	| TLet(na,e1,e2) -> TLet(nf na,f e1,f e2)
	| TLetRec(na,vs,e1,e2) -> TLetRec(nf na,mnf vs,f e1,f e2)
	| TApp(e1,es) -> TApp(f e1,mf es)
	| TTuple(es) -> TTuple(mf es)
	| TLetTuple(vs,e1,e2) -> TLetTuple(mnf vs,f e1,f e2)
	in
		(tast,(ty_subst subs nt,deb))
