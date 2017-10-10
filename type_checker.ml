open Syntax
open Debug

type tyvar = int

type ty =
	| TyInt
	| TyFloat
	| TyBool
	| TyVar of tyvar
	| TyArr of ty
	| TyFun of (ty list) * ty
	| TyTuple of ty list

exception TypeError of ty * ty * Debug.debug_data

let tyvar2str v = Printf.sprintf "'a%d" v

let rec type2str_with_pa t =
        match t with
        | TyInt | TyBool | TyFloat | TyVar _ -> type2str t
        | TyFun _ | TyArr _ | TyTuple _ -> "(" ^ (type2str t) ^ ")"

and type2str t =
	match t with
	| TyInt -> "int"
	| TyBool -> "bool"
	| TyFloat -> "float"
	| TyVar v -> tyvar2str v
	| TyFun (t1, t2) -> (
		Printf.sprintf "%s -> %s" 
			(String.concat " -> " (List.map type2str_with_pa t1))
			(type2str_with_pa t2)
		)
	| TyArr t -> Printf.sprintf "Array %s" (type2str_with_pa t)
	| TyTuple ts -> (
			match ts with
			| [] -> "()"
			| x :: xs -> 
				List.fold_left (fun r -> fun t -> r ^ " * " ^ (type2str_with_pa t)) (type2str_with_pa x) xs
		)

let print_type t = print_string ((type2str t) ^ "\n")


let genvar = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@t_%d" !c)

let gentype = let c = ref 0 in (fun () -> c := (!c)+1; TyVar(!c))

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


let genv = [
(* アセンブラで実装した*)
	("fless",TyFun([TyFloat;TyFloat],TyInt));
	("int_of_float",TyFun([TyFloat],TyInt));
	("float_of_int",TyFun([TyInt],TyFloat));
	("print_char",TyFun([TyInt],TyTuple([])));
	("read_char",TyFun([TyTuple([])],TyInt));
	("fiszero",TyFun([TyFloat],TyInt));
	("fispos",TyFun([TyFloat],TyInt));
	("fisneg",TyFun([TyFloat],TyInt));
	("fabs",TyFun([TyFloat],TyFloat));
	("floor",TyFun([TyFloat],TyFloat));
	("fsqr",TyFun([TyFloat],TyFloat)); (* x -> x^2 のほう *)
	("fneg",TyFun([TyFloat],TyFloat));
	("fhalf",TyFun([TyFloat],TyFloat));
	("sqrt",TyFun([TyFloat],TyFloat));   (* x -> root x のほう *)
	("sin",TyFun([TyFloat],TyFloat));
	("cos",TyFun([TyFloat],TyFloat));
	("atan",TyFun([TyFloat],TyFloat));
(*
	mlで実装した
	("false",TyInt);
	("print_int",TyFun([TyInt],TyTuple([])));
	("read_int",TyFun([TyTuple([])],TyInt));
	("read_float",TyFun([TyTuple([])],TyFloat));
*)
]

let global_funcs = List.map fst genv

let zip2 = List.map2 (fun a -> fun b -> (a,b))
let zip3 vs ws = List.map2 (fun (a,b) -> fun c -> (a,b,c)) (zip2 vs ws)

(* ここで、型情報をastに載せる必要がある *)

(* astdebは、 ast, deb の組。 *)
(* 環境は、 (旧変数名,(新変数名,型)) *)
(* 返り値は、 (新ast,制約,型,debug情報) *)
(* 制約は、型1,型2,デバッグ情報 *)
let rec type_infer astdeb env = 
	let ast,deb = astdeb in
	let rast,rc,rt = (
	match ast with
	| EConst(CBool x) -> (TConst(CBool x),[],TyBool)
	| EConst(CFloat x) -> (TConst(CFloat x),[],TyFloat)
	| EConst(CInt x) -> (TConst(CInt x),[],TyInt)
	| EVar(x) -> (
			try (
				let tv,tt = List.assoc x env in
				(TVar((tv,(tt,deb))),[],tt)
			)
			with
				| Not_found -> raise (Failure("undefined variable " ^ x ^ " at " ^ (debug_data2str deb)))
		)
	| EOp(op,vs) -> (
			let tects = List.map (fun x -> type_infer x env) vs in
			let tes = List.map (fun (x,_,_,_) -> x) tects in
			let tcs = List.concat (List.map (fun (_,x,_,_) -> x) tects) in
			let tts = List.map (fun (_,_,x,_) -> x) tects in 
			let tds = List.map (fun (_,_,_,x) -> x) tects in 
			
			let tyf = (match op with
			| Onot -> (fun () -> ([TyInt],TyInt))
			| Ominus -> (fun () -> ([TyInt],TyInt))
			| Oadd -> (fun () -> ([TyInt;TyInt],TyInt))
			| Osub -> (fun () -> ([TyInt;TyInt],TyInt))
			| Omul -> (fun () -> ([TyInt;TyInt],TyInt))
			| Odiv -> (fun () -> ([TyInt;TyInt],TyInt))
			| Ofadd -> (fun () -> ([TyFloat;TyFloat],TyFloat))
			| Ofsub -> (fun () -> ([TyFloat;TyFloat],TyFloat))
			| Ofmul -> (fun () -> ([TyFloat;TyFloat],TyFloat))
			| Ofdiv -> (fun () -> ([TyFloat;TyFloat],TyFloat))
			| Oeq -> (fun () -> ([TyInt;TyInt],TyInt))
			| Oneq -> (fun () -> ([TyInt;TyInt],TyInt))
			| Olt -> (fun () -> ([TyInt;TyInt],TyInt))
			| Oleq -> (fun () -> ([TyInt;TyInt],TyInt))
			| Ogt -> (fun () -> ([TyInt;TyInt],TyInt))
			| Ogeq -> (fun () -> ([TyInt;TyInt],TyInt))
			
			| Osemi2 -> (fun () -> let x = gentype () in ([TyTuple([]);x],x))
			| Osemi1 -> (fun () -> let x = gentype () in ([x],x))
			| OArrCrt -> (fun () -> let x = gentype () in ([TyInt;x],TyArr(x)))
			| OArrRead -> (fun () -> let x = gentype () in ([TyArr(x);TyInt],x))
			| OArrWrite -> (fun () -> let x = gentype () in ([TyArr(x);TyInt;x],TyTuple([])))
			) in
			let nts,rt = tyf () in
				(TOp(op,tes),(zip3 nts tts tds) @ tcs,rt)
		)
	| EIf(e1,e2,e3) -> (
			let te1,c1,tt1,deb1 = type_infer e1 env in
			let te2,c2,tt2,_ = type_infer e2 env in
			let te3,c3,tt3,deb3 = type_infer e3 env in
				(TIf(te1,te2,te3),(tt1,TyInt,deb1) :: (tt2,tt3,deb3) :: c1 @ c2 @ c3,tt2)
		)
	| ELet(n1,e2,e3) -> (
			let tn1 = (genvar ()) ^ "_" ^ n1 in
			let te2,c2,tt2,deb2 = type_infer e2 env in
			let te3,c3,tt3,_ = type_infer e3 ((n1,(tn1,tt2)) :: env) in
			(TLet((tn1,(tt2,deb2)),te2,te3),c2 @ c3,tt3)
		)
	| ELetRec(f1,ns,e2,e3) -> (
			(* fのα変換 *)
			let fn1 = (genvar ()) ^ "_" ^ f1 in
			(* fの返り値型 *)
			let fn1rt = gentype () in
			(* nsのα変換と、 型変数を作ったもの *)
			let tns = List.map (fun x -> (x,(genvar (),gentype ()))) ns in
			(* fの型 *)
			let f1t = TyFun(List.map (fun (_,(_,x)) -> x) tns,fn1rt) in
			(* e2についての推論 *) 
			let te2,c2,tt2,deb2 = type_infer e2 (tns @ [(f1,(fn1,f1t))] @ env) in
			(* これは、関数名より変数名の方が先に調べられるみたい *) 
			(* e3についての推論 *)
			let te3,c3,tt3,_ = type_infer e3 ((f1,(fn1,f1t)) :: env) in
			(TLetRec((fn1,(f1t,deb2)),List.map (fun (_,(x,t)) -> (x,(t,deb2))) tns,te2,te3),(tt2,fn1rt,deb2) :: c2 @ c3,tt3)
		)
	| EApp(e1,e2) -> (
		(* カリー化できないのでがんばる *)
			let te1,c1,tt1,deb1 = type_infer e1 env in
			let rt = gentype () in
			let tet = List.map (fun x -> type_infer x env) e2 in
			let funt = TyFun(List.map (fun (_,_,x,_) -> x) tet,rt) in
				(TApp(te1,List.map (fun (x,_,_,_) -> x) tet),
				 (tt1,funt,deb1) :: List.concat (List.map (fun (_,x,_,_) -> x) tet),
				 rt)
		)
	| ETuple(et) -> (
		(* 制約集合の和をとるだけにする。 *)
			let tet = List.map (fun x -> type_infer x env) et in
				(TTuple(List.map (fun (x,_,_,_) -> x) tet),
				 List.concat (List.map (fun (_,x,_,_) -> x) tet),
				 TyTuple(List.map (fun (_,_,x,_) -> x) tet))
		)
	| ELetTuple(ns,e1,e2) -> (
			let te1,c1,tt1,deb1 = type_infer e1 env in
			let tns = List.map (fun x -> (x,(genvar (),gentype ()))) ns in
			let te2,c2,tt2,_ = type_infer e2 (tns @ env) in
				(TLetTuple(List.map (fun (_,(x,t)) -> (x,(t,deb1))) tns,te1,te2),
				(tt1,TyTuple(List.map (fun (_,(_,x)) -> x) tns),deb1) :: c1 @ c2,tt2)
		)
	) in
	((rast,(rt,deb)),rc,rt,deb)

let rec print_constrs_sub cs =
        match cs with
        | [] -> ()
        | (x,y,_) :: xs -> (
        	Printf.printf "(%s , %s)\n" 
                (type2str x) (type2str y);
                print_constrs_sub xs)

let print_constrs cs =
        print_string "[\n";
        print_constrs_sub cs;
        print_string "]\n"

let rec ty_var_appear t v =
        match t with
        | TyInt | TyBool | TyFloat -> false
        | TyFun (t1s, t2) -> List.exists (fun x -> ty_var_appear x v) (t2 :: t1s)
        | TyVar x -> x = v
        | TyArr t -> (ty_var_appear t v)
        | TyTuple ts -> List.fold_left (fun r -> fun t -> r || (ty_var_appear t v)) false ts

let rec ty_subst (na,tt) t = 
	match t with
	| TyInt | TyFloat | TyBool -> t
	| TyVar(nb) -> if na == nb then tt else t
	| TyArr x -> TyArr(ty_subst (na,tt) x )
	| TyFun(ps,q) -> TyFun(List.map (fun p -> ty_subst (na,tt) p) ps,ty_subst (na,tt) q)
	| TyTuple ps -> TyTuple(List.map (fun x -> ty_subst (na,tt) x) ps) 

let rec constrs_subst s cs =
        match cs with
        | [] -> []
        | (x,y,d) :: xs -> (ty_subst s x,ty_subst s y,d) :: (constrs_subst s xs)

let rec unify cs = 
	(* print_constrs cs; *)
	match cs with
	| [] -> []
	| (t1,t2,deb) :: xs -> if t1 == t2 then unify xs else (
		try 
			match t1,t2 with
			| TyVar x,y | y,TyVar x -> (
					if ty_var_appear y x then 
						raise (TypeError(t1,t2,deb)) else 
						(x,y) :: (unify (constrs_subst (x,y) xs))
				)
			| TyArr a,TyArr b -> unify ((a,b,deb) :: xs)
			| TyFun(vs,b),TyFun(cs,d) -> unify ((b,d,deb) :: (List.map2 (fun a -> fun c -> (a,c,deb)) vs cs) @ xs)
			| TyTuple ps,TyTuple qs -> unify ((List.map2 (fun a b -> (a,b,deb)) ps qs) @ xs)
			| _ -> raise (TypeError(t1,t2,deb))
		with 
			| Invalid_argument("List.map2") -> raise (TypeError(t1,t2,deb))
	)


let rec ast_subst sub (ast,(nt,deb)) = 
	let f = ast_subst sub in
	let mf = List.map f in
	let nf (x,(t,d)) = (x,(ty_subst sub t,d)) in
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
  	(tast,(ty_subst sub nt,deb))
 
let ast_subst_rec subs ast = List.fold_left (fun r -> fun nsub -> ast_subst nsub r) ast subs 

let check ast = 
	try (
		let tast,tc,rt,_ = type_infer ast (List.map (fun (a,b) -> (a,(a,b))) genv) in
		(* print_type rt;
		print_constrs tc; *)
		let subs = unify tc in
			ast_subst_rec subs tast
	) with
		| TypeError(t1,t2,deb) -> 
			raise (Failure(Printf.sprintf 	
				"Type Unify Failed:\n %s \n with type %s and %s" (Debug.debug_data2str deb) (type2str t1) (type2str t2)))
