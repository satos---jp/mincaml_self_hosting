open Knorm
open Syntax
open Debug
open Genint

let gencname () = Printf.sprintf "@cls_%d" (genint ())

open Type
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

type globdef = {
	fn: name;
	cvs: name list;
	vs: name list;
	cbody: cexp; 
}

(* global�ɂȂ����֐��̃��X�g *)
let globals = ref ([] : globdef list)

(* global�ȕϐ��̃��X�g(�����the�Ȃ̂ŃN���[�W���ɓn���Ȃ��Ă悭�Ȃ�) *)
let globvars = ref ([] : string list)



let name2str (na,(ty,_)) = na ^ " : " ^ (type2str ty)

let vs2str vs = "(" ^ (String.concat " , " (List.map name2str vs)) ^ ")"

let rec cexp2str_base ast d = 
	match ast with
	| CConst x -> [(d,const2str x)]
	| COp(op,vs) -> [(d,(op2str op) ^ (vs2str vs))]
	| CLet(na,e1,e2) -> (d,"Let " ^ (name2str na) ^ " =") :: (cexp2str_base e1 (d+1)) @ [(d,"In")] @ (cexp2str_base e2 d)
	| CIf(ty,a,b,e1,e2) -> (d,"If " ^ (name2str a) ^ " " ^ (comptype2str ty) ^ " " ^ (name2str b) ^ " Then") :: (cexp2str_base e1 (d+1)) @ [(d,"Else")] @ (cexp2str_base e2 (d+1))
	| CVar(x) -> [(d,"Var " ^ (name2str x))]
	| CApp(fn,vs) -> [(d,"ClsApp " ^ (name2str fn) ^ (vs2str vs))]
	| CDirApp(fn,vs) -> [(d,"DirApp " ^ (name2str fn) ^ (vs2str vs))]
	| CTuple(vs) -> [(d,(vs2str vs))]
	| CLetTuple(vs,tn,e1) -> (d,"Let " ^ (vs2str vs) ^ " = " ^ (name2str tn)) :: [(d,"In")] @ (cexp2str_base e1 d)
	| CClosure(na,vs) -> [(d,"Closure <" ^ (name2str na) ^ "," ^ (vs2str vs) ^ ">")]

let cexp2str ast = 
	let ss = cexp2str_base ast 0 in
		(String.concat "\n" (List.map (fun (d,s) -> (String.make (d*2) ' ') ^ s) ss)) ^ "\n"

let def2str {fn=(fn,_); cvs=cvs; vs=vs; cbody=body} = 
	fn ^ " : Func " ^ (vs2str cvs) ^ (vs2str vs) ^ "[\n" ^ (cexp2str body) ^ "]\n"


let clos2str (gs,globvars,v) = 
	"globvars:\n" ^ 
	(String.concat "\n" globvars) ^ "\n" ^
	(String.concat "" (List.map def2str (List.rev gs))) 
	^ (cexp2str v) ^ "\n" 


(* ast����env�ɂȂ��ϐ���fv�ł���
	 globvers�������悤�ɂ���
 *)
let rec get_fvs ast (env : string list) = 
	let filter vs = List.filter (fun (x,_) -> not (List.mem x (env @ !globvars))) vs in
	(* Printf.printf "Ast ::\n%s\n" (cexp2str ast);
	Printf.printf "Env :: %s\n" (String.concat "," env); *)
	match ast with
	| KConst _ -> []
	| KOp(_,vs) | KTuple vs -> filter vs
	| KIf(_,x,y,e1,e2) -> (filter [x;y]) @ (get_fvs e1 env) @ (get_fvs e2 env)
	| KLet((x,_),e1,e2) -> (get_fvs e1 env) @ (get_fvs e2 (x :: env))
	| KLetRec((na,_),vs,e1,e2) -> (
			(get_fvs e1 (na :: (List.map fst vs) @ env)) @ (get_fvs e2 (na :: env)) 
			(* �֐�����global�ɂȂ�̂Ŏ����Ă��� -> �ق�܂��H(����񂩂��� -> �Ƃ�Ƃ���� )  *)
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

�̂Ƃ��Af��x�����炤�K�v������B

let ...
	let x = ...
	let f p = 
	   x ...
	let g q = 
	   f geeg

�̂Ƃ��Ag��x�����炤�K�v������B

�E fn �� e1,e2���ɑ��݂��邩���Ȃ̂ŁAfn�̃N���[�W����e1,e2 �ɍ��B
�E e1,e2���́Aglobal�ɂȂ���̂́A���ŃN���[�W���̊֐����g���Ă���ꍇ�A�錾���ɂ��̊֐��̃N���[�W���������Ă����K�v������B
�E ���̊֐��̃N���[�W���������Ƃɂ��A���̃N���[�W���쐬���ɕK�v�Ȉ����������Ŏ����Ă����K�v������(�܂��͐e���N���[�W����n���΂悢���H)


let ...
	let x = ...
	let f p = 
	   x ...
	let g q = 
	   f ...
	let h r = 
		 g ...

�̂Ƃ��A
g��f�̃N���[�W���������Ƃ��Ă��炤�B
h��g�̃N���[�W���������Ƃ��Ă��炤�B



�Ƃ肠�����A�錾���ɂ����ɃN���[�W���𔭐������āA�Ă�ł���l�ɂ͓K�X���̐l�̃N���[�W�������ɒǉ����Ă����Ƃ悳�����H

*)


(* 
	known�ɂ̓N���[�W�����s�v�Ȋ֐��̈ꗗ�������Ă���
*)
let rec remove_closure known istoplevel ast =
	let reccall = remove_closure known istoplevel in
	(* �N���[�W�����s�v�ȓz��l�Ƃ��ĎQ�Ƃ��Ă���Ȃ�A��N���[�W�������K�v������ *)
	let conv (a,d) cf = 
		try
			let (tn,f) = List.assoc a known in
			f (cf (tn,d))
		with
			| Not_found -> (cf (a,d))
	in
	(* conv�̑΃��X�g�� *)
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
			(* ���R�ϐ����W -> closure�ϊ��A���������� *)
			(* te1���ɏo�Ă���A�O���R���̂��̂��W�߂�  *)
			let fvs = unique_name (get_fvs e1 (fn :: (List.map fst known) @ (List.map fst args) @ (global_funcs ()))) in
			if List.length fvs = 0 then (
				Printf.printf "%s is closure free\n" fn;
				(* �N���[�W����(����)�s�v�B �ϐ��ɑ�������ۂɑ�ςɂȂ� *)
				let cn = gencname () in (* �e�N���[�W���ŁA�ꎞ�I�ɍ���� *)
				let tknown = (fn,(cn,fun x -> CLet((cn,ft),CClosure((fn,ft),[]),x))) :: known in
				let te1 = remove_closure tknown false e1 in
					globals := {fn = (fn,ft); cvs = fvs; vs = args; cbody=te1} :: !globals;
					remove_closure tknown istoplevel e2
			) else (
				(* fvs�ɃN���[�W���̂��߂̈����ꗗ�������Ă��āA������K��ɂ���Ă��� *)
				let global_name = gencname () in (* global�ł̖��O *)
				(* ����̓����ŌĂ΂��֐��ɂ́A�Ƃ肠�����S�Ă��̏������{���Ă���(�֐����o�����Ȃ����̂ɂ͖��ʂ���) *)
				
				(* TODO alpha�ϊ��������Ă�������(cfg�̍ۂɖ��O���������ƍ���) *)
				(* �Ƃ肠�����Acfg�̂ق��łǂ��ɂ����� *)
				let to_add_closure = fun x -> CLet((fn,ft),CClosure((global_name,ft),fvs),x) in
				(* t1��ϊ����� *)
				let te1 = to_add_closure (reccall e1) in
				(* �ċA�Ăяo���̂��߁A���g�̓���ȃN���[�W�������B��Xedi�œn���B *)
					(* Printf.printf "name %s :: type %s\n" fn (type2str ft); *)
					print_string (String.concat " : " (List.map fst fvs));
					Printf.printf " :: %s .aka %s\n" global_name fn;
					globals := {fn = (global_name,ft); cvs=fvs; vs=args; cbody=te1} :: !globals;
					to_add_closure (reccall e2)
			)
		)
	| KConst(a) -> CConst(a)
	| KOp(a,b) -> convv b (fun y -> COp(a,y))
	| KIf(cmp,a,b,c,d) -> conv a (fun x -> conv b (fun y -> CIf(cmp,x,y,reccall c,reccall d)))
	(* toplevel�Ȃ�����Ă��� *)
	| KLet((a,_) as na,b,c) -> (
			(if istoplevel then globvars := a :: !globvars else ());
			CLet(na,reccall b,reccall c)
		)
	| KVar(a) -> conv a (fun x -> CVar(x))
	| KTuple(a) -> convv a (fun x -> CTuple(x))
	| KLetTuple(a,b,c) -> conv b (fun y -> CLetTuple(a,y,reccall c))
	(* App�̊֐������͕ϊ����Ȃ� *)
	| KApp((a,_) as ad,b) -> (
			if List.mem a (List.map fst known) then 
				convv b (fun y -> CDirApp(ad,y))
			else
				convv b (fun y -> CApp(ad,y))
		)

let conv ast = 
	globals := [];
	globvars := [];
	let ta = remove_closure [] true ast in (!globals,!globvars,ta)


