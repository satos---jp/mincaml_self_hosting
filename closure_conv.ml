open Knorm
open Syntax

let gencname = let c = ref 0 in (fun () -> c := (!c)+1; Printf.sprintf "@cls_%d" !c)


(*
別に型を定義するのが億劫になってきたので
とりあえずKNormでやっていく。
*)


open Type_checker
type name = string * ty

type cexp = 
	| CConst of Syntax.const
	| COp        of Syntax.optype * (name list)
	| CLet       of name * cexp * cexp
	| CIf        of comptype * name * name * cexp * cexp
	| CVar       of name
	| CApp       of name * (name list)
	| CTuple     of (name list)
	| CLetTuple  of (name list) * name * cexp
	| CClosure   of name * (name list)

type globdef = (name list) * (name list) * cexp

(* name list *)
let globals = ref ([] : (name * globdef) list)

(*
program .. cexp,globdef list 

*)


let vs2str vs = "(" ^ (String.concat " , " (List.map name2str vs)) ^ ")"


let rec cexp2str_base ast d = 
	match ast with
	| CConst x -> [(d,const2str x)]
	| COp(op,vs) -> [(d,(op2str op) ^ (vs2str vs))]
	| CLet(na,e1,e2) -> (d,"Let " ^ (name2str na) ^ " =") :: (cexp2str_base e1 (d+1)) @ [(d,"In")] @ (cexp2str_base e2 (d+1))
	| CIf(ty,a,b,e1,e2) -> (d,"If " ^ (name2str a) ^ " " ^ (comptype2str ty) ^ " " ^ (name2str b) ^ " Then") :: (cexp2str_base e1 (d+1)) @ [(d,"Else")] @ (cexp2str_base e2 (d+1))
	| CVar(x) -> [(d,"Var " ^ (name2str x))]
	| CApp(fn,vs) -> [(d,"App " ^ (name2str fn) ^ (vs2str vs))]
	| CTuple(vs) -> [(d,(vs2str vs))]
	| CLetTuple(vs,tn,e1) -> (d,"Let " ^ (vs2str vs) ^ " = " ^ (name2str tn)) :: [(d,"In")] @ (cexp2str_base e1 (d+1))
	| CClosure(na,vs) -> [(d,"Closure <" ^ (name2str na) ^ "," ^ (vs2str vs) ^ ">")]

let cexp2str ast = 
	let ss = cexp2str_base ast 0 in
		String.concat "\n" (List.map (fun (d,s) -> (String.make (d*2) ' ') ^ s) ss) ^ "\n"
		

let name2str (x,t) = x ^ " : " ^ (type2str t)

let def2str (vs1,vs2,e) = "Func " ^ (vs2str vs1) ^ (vs2str vs2) ^ "[\n" ^ (cexp2str e) ^ "]\n"



let clos2str (gs,v) = 
	(String.concat "" (List.map (fun (x,d) -> (name2str x) ^ "\n" ^ (def2str d)) (List.rev gs))) 
	^ (cexp2str v) ^ "\n"

let rec get_fvs ast env = 
	let filter vs = List.filter (fun (x,_) -> not (List.mem x env)) vs in
	(* Printf.printf "Ast ::\n%s\n" (cexp2str ast);
	Printf.printf "Env :: %s\n" (String.concat "," env); *)
	match ast with
	| CConst _ -> []
	| COp(_,vs) | CTuple vs | CClosure(_,vs) -> filter vs
	| CIf(_,x,y,e1,e2) -> (filter [x;y]) @ (get_fvs e1 env) @ (get_fvs e2 env)
	| CLet((x,_),e1,e2) -> (get_fvs e1 env) @ (get_fvs e2 (x :: env))
	| CApp(f,vs) -> filter (f :: vs)
	| CLetTuple(vs,tp,e1) -> (filter [tp]) @ (get_fvs e1 ((List.map fst vs) @ env))
	| CVar x -> (filter [x])



let rec remove_closure ast env = 
	match ast with
	| KLetRec((fn,ft),args,e1,e2) -> (
			let te1 = remove_closure e1 env in
			(* e1中に出ている、外側由来のものを加える  *)
			let fvs = get_fvs te1 (env @ (List.map fst args) @ global_funcs) in
			(* fvsのうち、globalsは除いてよい *)
			let rfvs = List.filter (fun (x,_) -> not (List.mem x (List.map (fun ((x,_),_) -> x) !globals))) fvs in
			(* 実装面倒なのでとりあえず全部クロージャで。 2つの値を持っておく。 *)
			let global_name = gencname () in (* globalでの名前 *)
			let realfunty = ft in (* ここ、クロージャの引数の型も持っとく必要がありそう？ *)
				(* Printf.printf "name %s :: type %s\n" fn (type2str ft); *)
				globals := ((global_name,realfunty),(rfvs,args,te1)) :: !globals;
				CLet((fn,ft),CClosure((global_name,realfunty),rfvs),remove_closure e2 (fn :: env))
		)
	| KConst(a) -> CConst(a)
	| KOp(a,b) -> COp(a,b)
	| KIfEq(a,b,c,d) -> CIf(CmpEq,a,b,remove_closure c env,remove_closure d env)
	| KIfLeq(a,b,c,d) -> CIf(CmpLt,a,b,remove_closure c env,remove_closure d env)
	| KLet((a,t),b,c) -> CLet((a,t),remove_closure b env,remove_closure c (a :: env))
	| KVar(a) -> CVar(a)
	| KTuple(a) -> CTuple(a)
	| KLetTuple(a,b,c) -> CLetTuple(a,b,remove_closure c ((List.map fst a) @ env))
	| KApp(a,b) -> CApp(a,b)

let conv ast = 
	let ta = remove_closure ast [] in (!globals,ta)


