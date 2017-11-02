open Knorm
open Syntax
open Debug
open Type_checker
open Elim_unused
open Alpha
open Genint

type name = string * (ty * debug_data)

let gencname () = Printf.sprintf "@lift_%d" (genint ())

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

(*
先にe1を変換してからでないと、
let f x = 
	... in
let g x = 
	f x in
...
的な時に、fがliftされる場合、g内でfのlift後の名前を呼び出すことになるので、それが必要になってくる。

*)

let rec lift_func globs ast =
	(* 出せそうなら、出す。 *) 
	let reccall = lift_func globs in
	match ast with
	| KLetRec((fn,(ft,fd)),args,e1,e2) -> (
			let te1 = reccall e1 in
			(* e1中に出ている、外側由来のものを集める *)
			let fvs = unique_name (get_fvs te1 ((List.map fst args) @ (global_funcs ()))) in
			(* 自分自身が再帰的に、部分適用的に呼ばれている場合のみ、外に持ち上げられない *)
			(* いまのところは、保守的に、再帰があればやめておく。(ちゃんとするにはte1を二度とる必要があるはず) *)
			if not (List.mem fn (List.map fst fvs)) then (
				Printf.printf "lift %s\n" fn;
				let liftn = (gencname ())^ fn in (* 持ち上げ後の名前 *)
				let tft = ( (* 持ち上げ後の型 *)
					match ft with
					| TyFun(fts,tt) -> TyFun((List.map (fun (_,(t,_)) -> t) fvs) @ fts,tt)
					| _ -> raise (Failure (Printf.sprintf "LetRec %s invalid type %s : %s" fn (type2str ft) (debug_data2str fd)))
				) in
				let tgs = (fn,(liftn,tft,fd,fvs)) :: globs in
					globals := ((liftn,(tft,fd)),(fvs @ args,te1)) :: !globals;
					KLetRec((fn,(ft,fd)),args,te1,lift_func tgs e2)	
			)
			else KLetRec((fn,(ft,fd)),args,te1,reccall e2)	
		)
	| KApp((a,d),nvs) -> (
			try 
				let tn,tt,td,vs = List.assoc a globs in
					Printf.printf "App %s\n" a;
					KApp((tn,(tt,td)),vs @ nvs)
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

