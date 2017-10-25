open Knorm
open Syntax
open Debug
open Type_checker
open Elim_unused
open Alpha

let gencname = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@lift_%d" !c)

(* ast中のenvにない変数がfvである *)
let rec get_fvs ast (env : string list) = 
	let filter vs = List.filter (fun (x,_) -> not (List.mem x env)) vs in
	match ast with
	| KConst _ -> []
	| KOp(_,vs) | KTuple vs -> filter vs
	| KIf(_,x,y,e1,e2) -> (filter [x;y]) @ (get_fvs e1 env) @ (get_fvs e2 env)
	| KLet((x,_),e1,e2) -> (get_fvs e1 env) @ (get_fvs e2 (x :: env))
	| KLetRec((na,_),vs,e1,e2) -> (
			(get_fvs e1 (na :: (List.map fst vs) @ env)) @ (get_fvs e2 (na :: env)) 
			(* 関数名はglobalになるので持っておく -> ほんまか？(いらんかった -> とれとらんやんけ )  *)
		)
	| KApp(f,vs) -> filter (f :: vs)
	| KLetTuple(vs,tp,e1) -> (filter [tp]) @ (get_fvs e1 ((List.map fst vs) @ env))
	| KVar x -> (filter [x])


(* 今回は、λリフティングしたやつをゆるやかにグローバルに入れる。 *)
type globdef = (name list) * kexp
let globals = ref ([] : (name * globdef) list)


let rec unique_name vs =
	match vs with
	| [] -> []
	| (x,dt) :: xs -> 
		let txs = unique_name xs in
			if List.exists (fun (y,_) -> x = y) txs then txs else (x,dt) :: txs


let rec lift_func globs ast =
	(* 出せそうなら、出す。 *) 
	let reccall = lift_func globs in
	match ast with
	| KLetRec((fn,ft),args,e1,e2) -> (
			(* 自由変数収集 -> closure変換、が正しそう *)
			(* te1中に出ている、外側由来のものを集める  *)
			let fvs = unique_name (get_fvs e1 ((List.map fst args) @ global_funcs)) in
			(* 自分自身が再帰的に、部分適用的に呼ばれている場合のみ、外に持ち上げられない *)
			if not (List.mem fn (List.map fst fvs)) then (
				Printf.printf "lift %s\n" fn;
				let liftn = (gencname ())^ fn in (* 持ち上げ後の名前 *)
				let tgs = (fn,(fvs,liftn)) :: globs in
				let te1 = lift_func tgs e1 in
					globals := ((liftn,ft),(fvs @ args,te1)) :: !globals;
					KLetRec((fn,ft),args,te1,lift_func tgs e2)	
			)
			else KLetRec((fn,ft),args,reccall e1,reccall e2)	
		)
	| KApp((a,d),nvs) -> (
			try 
				let vs,tn = List.assoc a globs in
					Printf.printf "App %s\n" a;
					KApp((tn,d),vs @ nvs)
			with
				| Not_found -> ast
		)
	| KIf(cmp,a,b,c,d) -> KIf(cmp,a,b,reccall c,reccall d)
	| KLet(a,b,c) -> KLet(a,reccall b,reccall c)
	| KLetTuple(a,b,c) -> KLetTuple(a,b,reccall c)
	| _ -> ast

let lift ast = 
	let ta = lift_func [] ast in 
	let tast = List.fold_left (fun r -> fun (fn,(vs,bo)) -> KLetRec(fn,vs,bo,r)) ta (!globals) in
		elim_unused (alpha_conv tast [])

