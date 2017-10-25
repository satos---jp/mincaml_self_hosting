open Knorm
open Syntax
open Debug

let gencname = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@cls_%d" !c)

open Type_checker
type name = string * (ty * debug_data)

type cexp = 
	| CConst of Syntax.const
	| COp        of Syntax.optype * (name list)
	| CLet       of name * cexp * cexp
	| CIf        of comptype * name * name * cexp * cexp
	| CVar       of name
	| CApp       of name * (name list)
	| CDirApp    of name * (name list)
	| CTuple     of (name list)
	| CLetTuple  of (name list) * name * cexp
	| CClosure   of name * (name list)

type globdef = (name list) * (name list) * cexp

(* name list *)
let globals = ref ([] : (name * globdef) list)

(*
program .. cexp,globdef list 
*)

let name2str (na,(ty,_)) = na ^ " : " ^ (type2str ty)

let vs2str vs = "(" ^ (String.concat " , " (List.map name2str vs)) ^ ")"

let rec cexp2str_base ast d = 
	match ast with
	| CConst x -> [(d,const2str x)]
	| COp(op,vs) -> [(d,(op2str op) ^ (vs2str vs))]
	| CLet(na,e1,e2) -> (d,"Let " ^ (name2str na) ^ " =") :: (cexp2str_base e1 (d+1)) @ [(d,"In")] @ (cexp2str_base e2 (d+1))
	| CIf(ty,a,b,e1,e2) -> (d,"If " ^ (name2str a) ^ " " ^ (comptype2str ty) ^ " " ^ (name2str b) ^ " Then") :: (cexp2str_base e1 (d+1)) @ [(d,"Else")] @ (cexp2str_base e2 (d+1))
	| CVar(x) -> [(d,"Var " ^ (name2str x))]
	| CApp(fn,vs) -> [(d,"App " ^ (name2str fn) ^ (vs2str vs))]
	| CDirApp(fn,vs) -> [(d,"DirApp " ^ (name2str fn) ^ (vs2str vs))]
	| CTuple(vs) -> [(d,(vs2str vs))]
	| CLetTuple(vs,tn,e1) -> (d,"Let " ^ (vs2str vs) ^ " = " ^ (name2str tn)) :: [(d,"In")] @ (cexp2str_base e1 (d+1))
	| CClosure(na,vs) -> [(d,"Closure <" ^ (name2str na) ^ "," ^ (vs2str vs) ^ ">")]

let cexp2str ast = 
	let ss = cexp2str_base ast 0 in
		(String.concat "\n" (List.map (fun (d,s) -> (String.make (d*2) ' ') ^ s) ss)) ^ "\n"
		
let def2str (vs1,vs2,e) = "Func " ^ (vs2str vs1) ^ (vs2str vs2) ^ "[\n" ^ (cexp2str e) ^ "]\n"

let clos2str (gs,v) = 
	(String.concat "" (List.map (fun ((x,_),bo) -> x ^ " : " ^ (def2str bo)) (List.rev gs))) 
	^ (cexp2str v) ^ "\n"

(* ast中のenvにない変数がfvである *)
let rec get_fvs ast (env : string list) = 
	let filter vs = List.filter (fun (x,_) -> not (List.mem x env)) vs in
	(* Printf.printf "Ast ::\n%s\n" (cexp2str ast);
	Printf.printf "Env :: %s\n" (String.concat "," env); *)
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

let rec unique_name vs =
	match vs with
	| [] -> []
	| (x,dt) :: xs -> 
		let txs = unique_name xs in
			if List.exists (fun (y,_) -> x = y) txs then txs else (x,dt) :: txs

(*

let ...
  let f p = 
    let g q = 
       x 

のとき、fはxをもらう必要がある。

let ...
	let x = ...
	let f p = 
	   x ...
	let g q = 
	   f geeg

のとき、gはxをもらう必要がある。

・ fn は e1,e2中に存在するかもなので、fnのクロージャをe1,e2 に作る。
・ e1,e2中の、globalになるものは、中でクロージャの関数を使っている場合、宣言時にその関数のクロージャを持っておく必要がある。
・ その関数のクロージャを持つことにより、そのクロージャ作成時に必要な引数を自分で持っておく必要がある(または親がクロージャを渡せばよいか？)


let ...
	let x = ...
	let f p = 
	   x ...
	let g q = 
	   f ...
	let h r = 
		 g ...

のとき、
gはfのクロージャを引数としてもらう。
hはgのクロージャを引数としてもらう。



とりあえず、宣言時にそこにクロージャを発生させて、呼んでいる人には適宜その人のクロージャ引数に追加していくとよさそう？

*)

let rec remove_closure known ast =
	let reccall = remove_closure known in
	let conv (a,d) cf = 
		try 
			let (tn,f) = List.assoc a known in
			f (cf (tn,d))
		with
			| Not_found -> (cf (a,d))
	in
	let convv vs cf = 
		let tvs,tf = (List.fold_right (fun (a,d) -> fun (rvs,rf) -> 
			try 
				let (tn,f) = List.assoc a known in
				((tn,d) :: rvs,(fun x -> f (rf x)))
			with
				| Not_found -> ((a,d) :: rvs,rf)
		) vs ([],fun x -> x))
		in
			tf (cf tvs)
	in 
	match ast with
	| KLetRec((fn,ft),args,e1,e2) -> (
			(* 自由変数収集 -> closure変換、が正しそう *)
			(* te1中に出ている、外側由来のものを集める  *)
			let fvs = unique_name (get_fvs e1 (fn :: (List.map fst known) @ (List.map fst args) @ global_funcs)) in
			if List.length fvs = 0 then (
				Printf.printf "%s is closure free\n" fn;
				(* クロージャは(普通)不要。 変数に代入される際に大変になる *)
				let cn = gencname () in (* 各クロージャで、一時的に作るやつ *)
				let tknown = (fn,(cn,fun x -> CLet((cn,ft),CClosure((fn,ft),[]),x))) :: known in
				let te1 = remove_closure tknown e1 in
					globals := ((fn,ft),(fvs,args,te1)) :: !globals;
					remove_closure tknown e2
			) else (
				(* fvsにクロージャのための引数一覧が入っていて、これを規約にやっていく *)
				let global_name = gencname () in (* globalでの名前 *)
				(* これの内側で呼ばれる関数には、とりあえず全てこの処理を施しておく(関数が出現しないものには無駄だが) *)
				let to_add_closure = fun x -> CLet((fn,ft),CClosure((global_name,ft),fvs),x) in
				(* t1を変換する *)
				let te1 = to_add_closure (reccall e1) in
				(* 再帰呼び出しのため、自身の特殊なクロージャを作る。後々ediで渡す。 *)
					(* Printf.printf "name %s :: type %s\n" fn (type2str ft); *)
					print_string (String.concat " : " (List.map fst fvs));
					Printf.printf " :: %s .aka %s\n" global_name fn;
					globals := ((global_name,ft),(fvs,args,te1)) :: !globals;
					to_add_closure (reccall e2)
			)
		)
	| KConst(a) -> CConst(a)
	| KOp(a,b) -> convv b (fun y -> COp(a,y))
	| KIf(cmp,a,b,c,d) -> conv a (fun x -> conv b (fun y -> CIf(cmp,x,y,reccall c,reccall d)))
	(*多分、α変換されてて大丈夫だよね...? *)	
	| KLet(a,b,c) -> CLet(a,reccall b,reccall c)
	| KVar(a) -> conv a (fun x -> CVar(x))
	| KTuple(a) -> convv a (fun x -> CTuple(x))
	| KLetTuple(a,b,c) -> conv b (fun y -> CLetTuple(a,y,reccall c))
	(* Appの関数だけは変換しない *)
	| KApp((a,_) as ad,b) -> (
			if List.mem a (List.map fst known) then 
				convv b (fun y -> CDirApp(ad,y))
			else
				convv b (fun y -> CApp(ad,y))
		)

let conv ast = 
	let ta = remove_closure [] ast in (!globals,ta)


