open Main_option
open Source2ast
open Spec
open Type
open Type_expr
open Syntax
open Debug


let externs = ref []

let genv_base = List.map (fun (na,t) -> (na,schemize t [] [])) (
[ (* lib.s *)
	("int_of_float",TyFun([TyFloat],TyInt));
	("float_of_int",TyFun([TyInt],TyFloat));
	(* れいず!! *)
	("raise_match_failure",TyFun([TyTuple([])],TyTuple([])));

] @ [ (* lib_string.s *)

	("String@@",TyFun([TyStr;TyStr],TyStr));
	("String@length",TyFun([TyStr],TyInt));
	("String@get",TyFun([TyStr;TyInt],TyChar));

] @ [ (* char.s *)

	("Char@code",TyFun([TyChar],TyInt));
	("Char@chr",TyFun([TyInt],TyChar));
	("Char@escaped",TyFun([TyChar],TyStr));

] @ [ (* libio_linux.s *)

	("print_char",TyFun([TyInt],TyTuple([])));
	("print_char_err",TyFun([TyInt],TyTuple([])));
	("read_char",TyFun([TyTuple([])],TyInt));
	("print_string",TyFun([TyStr],TyTuple([]))); (* とりまアセンブラで *)

] @ [ (* printf.s *)
	(* TODO(satos) format型導入する *)
	(*
	("Printf@sprintf",TyFun([TyStr],TyVar(genint ())))
	ランク1多相導入しないとどうしようもなくなってきたな...???
	*)
])

let genv () = genv_base @ (!externs)


(*
float系はいったんよける
	("fless",TyFun([TyFloat;TyFloat],TyInt));
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
*)
	
let get_imports () = genv ()

let zip2 = List.map2 (fun a -> fun b -> (a,b))
let zip3 vs ws = List.map2 (fun (a,b) -> fun c -> (a,b,c)) (zip2 vs ws)



(*
# type a = H of (int * int * int);;
type a = H of (int * int * int)
# fun x -> match x with H p -> p;;
- : a -> int * int * int = <fun>

# type a = H of int * int * int;;
type a = H of int * int * int
# fun x -> match x with H p -> p;;
Error: The constructor H expects 3 argument(s),
       but is applied here to 1 argument(s)

まーた闇なんだが
*)

(*
# type b = Y of unit;; 
# fun x -> match x with Y() -> 3;;    (この () は value-name)
- : b -> int = <fun>
# fun x -> match x with Y(_) -> 3;;   (この () は  ( pattern )  のやつ)
- : b -> int = <fun>
# fun x -> match x with Y(_,_) -> 3;;
Error: This pattern matches values of type 'a * 'b
       but a pattern was expected which matches values of type unit

:thinking_face:
*)




(*
	型環境        env
	型定義環境    venv : (string * (int * ty * (ty list))) list
	パターン pat
	pat に対応する変数名 pna
*)

(* cond : texp, 
	 bind : texp -> texp,
	 tenv : (string * tyscheme) list,  :: 値多相制限あたりでここは制限されるぽい？
	 constr : (ty * ty * debug) list
を返す *)
(* マッチをif文の連鎖にするので、cond に成功したら (bind e),みたいなかんじで。 *)
let rec pattern2env env venv pat pna = 
	let self = pattern2env env venv in
	let _,td = pna in
	let pt,deb = td in
	let true_expr = TConst(CInt(1)),(TyInt,deb) in
	let false_expr = TConst(CInt(0)),(TyInt,deb) in
	let cmptag tagn = (
		TOp(Oeq,[
			(TConst(CInt(tagn)),(TyInt,deb));
			(TOp(OGetTuple(0),[pna]),(TyInt,deb))
		]),(TyInt,deb)
	) in
	match pat with
	| PVar(x) -> (
			true_expr,
			(fun ((_,ted) as e) -> TLet((x,td),pna,e),ted),
			[(x,no_fv_scheme pt)], (* TODO(satos) ここほんまかっつってる *)
			[]
		)
	| PVariant(tag) -> (
			let tagn,tagty,tagarg = (
				try
					(List.assoc tag venv) ()
				with
					| Not_found -> raise (Failure("Undefined variant tag " ^ tag ^ " at " ^ (debug_data2str deb)))
			) in
			let ls = List.length tagarg in
			if ls <> 0 then
				raise (Failure(Printf.sprintf "tag %s needs %d args at %s" tag ls (debug_data2str deb)))
			else (
				(cmptag tagn),
				(fun e -> e),
				[],
				[(pt,tagty,deb)]
			)
		)
	| PVariantApp(tag,ps) -> (
			let tagn,tagty,tagarg = (
				try
					(List.assoc tag venv) ()
				with
					| Not_found -> raise (Failure("Undefined variant tag " ^ tag ^ " at " ^ (debug_data2str deb)))
			) in
			let ls = List.length tagarg in
			ivprint (Printf.sprintf "%s :: %d\n" tag ls); 
			
			let nt,ftbind = (
				if ls == 1 then (
					List.hd tagarg,
					(fun nt -> 
							(TOp(OGetTuple(0),[
								TOp(OGetTuple(1),[pna]),(nt,deb)
							]),(TyTuple([nt]),deb)))
				) else (
					match ps with
					| PTuple rps when (List.length rps == ls) -> (
							TyTuple(tagarg),
							(fun nt -> (TOp(OGetTuple(1),[pna]),(nt,deb)))
						)
					| _ -> raise (Failure(Printf.sprintf "tag %s needs %d args at %s" tag ls (debug_data2str deb)))
				)
			) 
			in
			let na = (genvar (),(nt,deb)) in
			let cond, bind, tenv, cs = self ps (TVar(na),(nt,deb)) in
			let tbind = 
				(fun ((_,ted) as e) -> TLet(na,(ftbind nt),e),ted)
			in
			(
				(* TODO(satos) ここ多分冗長なので直す(よく分かってない) *)
				(TIf(cmptag tagn,tbind (bind cond),false_expr),(TyInt,deb)),
				(fun e -> tbind (bind e)),
				tenv,
				(pt,tagty,deb) :: cs
			)
		)
	| PTuple(ps) -> (
			let ((cond, bind, rtenv, cs),ts) = 
				List.fold_left (fun ((rcond, rbind, rtenv, rcs),ts) (i,pat) -> 
					let nt = gentype () in
					let na = (genvar (),(nt,deb)) in
					let cond, bind, tenv, cs = self pat (TVar(na),(nt,deb)) in
					((
						(TIf(rcond,cond,false_expr),(TyInt,deb)),
						(fun ((_,ted) as e) -> TLet(na,
							(TOp(OGetTuple(i),[pna]),(nt,deb))
						,(bind (rbind e))),ted),
						tenv @ rtenv,
						cs @ rcs
					),nt :: ts)
				) ((true_expr,(fun x -> x),env,[]),[]) (List.mapi (fun i x -> (i,x)) ps)
			in
				(cond, bind, rtenv, (TyTuple(List.rev ts),pt,deb) :: cs)
		)

let rec constrs2str_sub cs =
	match cs with
	| [] -> ""
	| (x,y,_) :: xs -> (
			(Printf.sprintf "(%s , %s)\n" (type2str x) (type2str y)) ^
      (constrs2str_sub xs)
     )

let constrs2str cs =
	"[\n" ^ (constrs2str_sub  cs) ^ "]\n"


(* ここで、型情報をastに載せる必要がある *)

(* astdebは、 ast, deb の組。 *)
(* 環境は、 (変数名,(型とデバッグ情報)) *)
(* 返り値は、 (新ast,制約,型,debug情報) *)
(* 制約は、型1,型2,デバッグ情報 *)
(* ここでは、まだα変換はしないことにしました *)

(* TyInt関連は最後にチェックする *)
let addglobalcs = ref []

let list2str v f =
	"[" ^ (
		let rec self w = 
			match w with
			| [] -> "]"
			| [x] -> (f x) ^ "]"
			| x :: xs -> (f x) ^ ";" ^ (self xs)
		in 
			self v
	)

let print_env v = 
	print_string "env\n";
	print_string (list2str v (fun (x,_) -> x));
	print_string "\n"

(*
let多相にするぞい。
env を (name * ty) から (name * ty_scheme) にする
*)


let rec type_infer tyenv venv astdeb env = 
	let self = type_infer tyenv venv in
	let ast,deb = astdeb in
	let rast,rc,rt = (
	match ast with
	| EConst(CFloat x) -> (TConst(CFloat x),[],TyFloat)
	| EConst(CInt x) -> (TConst(CInt x),[],TyInt)
	| EConst(CString x) -> (TConst(CString x),[],TyStr)
	| EConst(CChar x) -> (TConst(CChar x),[],TyChar)
	| EVar(x) -> (
			(*
			print_env env;
			Printf.printf " of %s\n" x; *)
			try (
				let tt = List.assoc x env in
				let ttt = instanciate tt in
				(TVar((x,(ttt,deb))),[],ttt)
			)
			with
				| Not_found -> raise (Failure("undefined variable " ^ x ^ " at " ^ (debug_data2str deb)))
		)
	| EOp(op,vs) -> (
			let tects = List.map (fun x -> self x env) vs in
			let tes = List.map (fun (x,_,_,_) -> x) tects in
			let tcs = List.concat (List.map (fun (_,x,_,_) -> x) tects) in
			let tts = List.map (fun (_,_,x,_) -> x) tects in 
			let tds = List.map (fun (_,_,_,x) -> x) tects in 
			
			let tyf = (match op with
			| Onot | Ominus | Oiadd _ | Oibysub _ | Oimul _ | Oibydiv _ -> (fun () -> ([TyInt],TyInt))
			| Oadd | Osub | Omul | Odiv -> (fun () -> ([TyInt;TyInt],TyInt))
			| Ofadd | Ofsub | Ofmul | Ofdiv -> (fun () -> ([TyFloat;TyFloat],TyFloat))
			
			(* 値をboxingすることにより、 stringも同値性が比較できるようにする(adhocok多相) *)
			(* 比較不能な奴(ex.関数) は、実行時に(!!)エラーが出るようにする *)
			| Oeq | Oneq -> (fun () -> let x = gentype () in ([x;x],TyInt))
			
			(* 全て、float同士、でもいけるようにする(多相性) *)
			| Olt | Oleq | Ogt | Ogeq -> (
				(fun () -> let x = gentype () in  
					addglobalcs := [(x,TyNum,deb)] @ !addglobalcs;
					([x;x],TyInt))
			)
			
			| Osemi2 -> (fun () -> let x = gentype () in ([TyTuple([]);x],x))
			| Osemi1 -> (fun () -> let x = gentype () in ([x],x))
			| OArrCrt -> (fun () -> let x = gentype () in ([TyInt;x],TyArr(x)))
			| OArrRead -> (fun () -> let x = gentype () in ([TyArr(x);TyInt],x))
			| OArrWrite -> (fun () -> let x = gentype () in ([TyArr(x);TyInt;x],TyTuple([])))
			| OiArrRead _ -> (fun () -> let x = gentype () in ([TyArr(x)],x))
			| OiArrWrite _ -> (fun () -> let x = gentype () in ([TyArr(x);x],TyTuple([])))
			
			| OGetTupleWithLen(ls,i) -> (
					fun () -> let xs = (let rec f t = if t = 0 then [] else (gentype ()) :: f (t-1) in f ls)
					in ([TyTuple(xs)],List.nth xs i)
				)
			| OSubTuple _ | OGetTuple _ -> raise (Failure (Printf.sprintf "%s shouldn't appear in parsed syntax" (op2str op)))
			) in
			let nts,rt = tyf () in
				(TOp(op,tes),(zip3 nts tts tds) @ tcs,rt)
		)
	| EIf(e1,e2,e3) -> (
			let te1,c1,tt1,deb1 = self e1 env in
			let te2,c2,tt2,_ = self e2 env in
			let te3,c3,tt3,deb3 = self e3 env in
				(TIf(te1,te2,te3),(tt1,TyInt,deb1) :: (tt2,tt3,deb3) :: c1 @ c2 @ c3,tt2)
		)
	| ELet(n1,e2,e3) -> (
			let te2,c2,tt2,deb2 = self e2 env in
			let subs = unify tyenv c2 in
			let tte2 = ast_subst subs te2 in
			let ttt2 = ty_subst subs tt2 in
			
			(* いったんenvをアップデートしないといけなかったりする。(都合で) *)
			let ts2 = schemize ttt2 env subs in

			print_string ("letdecl " ^ n1 ^ "\n");
			print_string (constrs2str c2);
			
			let te3,c3,tt3,_ = self e3 ((n1,ts2) :: env) in
			
			(* TODO(satos) デバッグ用の名前も、多分schemaにしないといけないんだけど面倒なので飛ばす *)
			(* これc2も外に出さなきゃいけないよね...??(毎回言ってんな...)(c2のうち、不要なのは消しといたほうがよさそう...(計算量的に)) *)
			(* これ正しくはenvもアップデートされますねここで!?(設計がやばくなりそう)*)
			(TLet((n1,(ttt2,deb2)),tte2,te3),c2 @ c3,tt3)
		)
	| ELetRec(f,ps,e2,e3) -> (
			let ns = List.map (fun x -> match x with | PVar v -> v | _ -> raise (Failure "Shouldn't reach here")) ps in
			
			let frt = gentype () in
			let btns = List.map (fun x -> (x,gentype ())) ns in
			let tns = List.map (fun (a,b) -> (a,no_fv_scheme b)) btns in
			let ft = TyFun(List.map (fun (_,x) -> x) btns,frt) in 
			let fts = no_fv_scheme ft in
			(* ここ、ftとtnsどっちも自由変数なしでいいんだっけ...??(いやさすがに制約がなくなりそうなのでよいか) *)
			let te2,c2,tt2,deb2 = self e2 (tns @ [(f,fts)] @ env) in
			(* これは、関数名より変数名の方が先に調べられるみたい *) 
			
			let cc2 = (tt2,frt,deb2) :: c2 in
			let subs = unify tyenv cc2 in
			let tte2 = ast_subst subs te2 in
			let tft = ty_subst subs ft in
			let tfts = schemize tft env subs in
			
			(* e3についての推論 *)
			let te3,c3,tt3,_ = self e3 ((f,tfts) :: env) in
			(
				TLetRec((f,(tft,deb2)),List.map (fun (x,t) -> (x,(ty_subst subs t,deb2))) btns,tte2,te3),
				c2 @ c3, (* やっぱcc2いるよなぁ... *)
				tt3
			)
		)
	| EApp(e1,e2) -> (
		(* カリー化できないのでがんばる *)
			let te1,c1,tt1,deb1 = self e1 env in
			let rt = gentype () in
			let tet = List.map (fun x -> self x env) e2 in
			(* ずっとバグってたみたいですね。 *)
			let funt = TyFun(List.map (fun (_,_,x,_) -> x) tet,rt) in
				(TApp(te1,List.map (fun (x,_,_,_) -> x) tet),
				 (tt1,funt,deb1) :: (List.concat (List.map (fun (_,x,_,_) -> x) tet)) @ c1,
				 rt)
		)
	| ETuple(et) -> (
		(* 制約集合の和をとるだけにする。 *)
			let tet = List.map (fun x -> self x env) et in
				(TTuple(List.map (fun (x,_,_,_) -> x) tet),
				 List.concat (List.map (fun (_,x,_,_) -> x) tet),
				 TyTuple(List.map (fun (_,_,x,_) -> x) tet))
		)
	| ELetTuple(ns,e1,e2) -> (
			let te1,c1,tt1,deb1 = self e1 env in
			(*
			let subs = unify tyenv c1 in
			let tte1 = ast_subst subs te1 in
			let ttt1 = ty_subst subs tt1 in
			let ts1 = schemize ttt1 env in
			TODO(satos) なんかここいる気がするがわかんない
			*)
			
			let btns = List.map (fun x -> (x,gentype ())) ns in
			let tns = List.map (fun (a,b) -> (a,no_fv_scheme b)) btns in
			
			let te2,c2,tt2,_ = self e2 (tns @ env) in
				(TLetTuple(List.map (fun (x,t) -> (x,(t,deb1))) btns,te1,te2),
				(tt1,TyTuple(List.map (fun (_,x) -> x) btns),deb1) :: c2,tt2)
		)
	| EVariant(tag,es) -> (
			let tagn,tagty,args = (
				try
					(List.assoc tag venv) ()
				with
					| Not_found -> raise (Failure("Undefined variant tag " ^ tag ^ " at " ^ (debug_data2str deb)))
			) in
			let tagne = TConst(CInt tagn),(TyInt,deb) in
			let tet = List.map (fun x -> self x env) es in
			let tt = TyTuple(List.map (fun (_,_,x,_) -> x) tet) in
			let tagcs = (
				try 
					List.map2 (fun t (_,_,x,_) -> (t,x,deb)) args tet 
				with
				| Invalid_argument("List.map2") -> raise (Failure(
						Printf.sprintf "tag %s expects %d args but given %d args at %s" tag (List.length args) (List.length tet) (debug_data2str deb)
					))
			) in
			(TTuple([tagne;(TTuple(List.map (fun (x,_,_,_) -> x) tet),(tt,deb))]),
			 tagcs @ List.concat (List.map (fun (_,x,_,_) -> x) tet),
			 tagty)
		)
	| EMatch(ep,ps) -> (
			(*
			# let p = match (fun f x -> x) with f -> (f 3,f true);;
			val p : ('_a -> '_a) * ('_b -> '_b) = (<fun>, <fun>)
			# let x = fst p;;
			val x : '_a -> '_a = <fun>
			# x 6;;
			- : int = 6
			# p;;
			- : (int -> int) * ('_a -> '_a) = (<fun>, <fun>)
			
			なんもわからん...(ここで多相の値制限が入るっぽい)(TODO(satos) あとで対応する)
			*)
			
			(* if文に訳する *)
			let te1,c1,tt1,deb1 = self ep env in
			let na = genvar (),(tt1,deb1) in
			let nav = TVar(na),(tt1,deb1) in
			(* ニュアンスは関数とその適用がめっちゃある感じ *)
			
			(* ここのletで多相はどうなるんだ...みたいな気分になる *) 
			let tet =  List.map (fun (pat,e) ->  
				let cond,bind,aenv,cs = pattern2env env venv pat nav in
				let tenv = aenv @ env in
				(cond,bind,cs),(self e tenv)
			) ps in
			
			let rt = gentype () in
			let ert = TyFun([gentype ()],rt) in
			let erre : texp = (TApp(
				(TVar(("raise_match_failure",(ert,deb))),(ert,deb)),
				(* ここで載せてみる *)
				[TConst(CString(debug_data2str deb)),(TyStr,deb)]
			),(rt,deb)) in
			let re = TLet(na,te1,(
				List.fold_right (fun ((cond,bind,_),(x,_,_,d)) ne -> 
					TIf(cond,bind x,ne),(rt,deb)
				) tet erre
			)) in 
			let rcs = (
				(List.map (fun (_,(_,_,t,_)) -> (t,rt,deb)) tet) @ 
				(List.concat (List.map (fun ((_,_,c1),(_,c2,_,_)) -> (c1 @ c2)) tet)) @ 
				c1
			) in
				(re,rcs,rt)
		)
	) in
	((rast,(rt,deb)),rc,rt,deb)








type ('a, 'b) either = Left of 'a | Right of 'b

let rec fix_partial_apply (ast,(t,deb)) =  
	let f = fix_partial_apply in
	let mf = List.map f in
	let tast = match ast with
	| TConst _ | TVar _ -> ast
	| TOp(op,es) -> TOp(op,mf es)
	| TIf(e1,e2,e3) -> TIf(f e1,f e2,f e3)
	| TLet(na,e1,e2) -> TLet(na,f e1,f e2)
	| TLetRec(na,vs,e1,e2) -> TLetRec(na,vs,f e1,f e2)
	| TApp(e1,e2) -> (
		let ((_,(t,d) as td) as ftd) = f e1 in
		let es = mf e2 in
		let raise_invalid () = 
			raise (Failure(Printf.sprintf "Invalid apply for %s with (%s) at %s" 
					(type2str t) (String.concat ", " (List.map (fun (_,(t,_)) -> (type2str t)) es)) (debug_data2str d)
			))
		in
		let rtes,rd = (
				match t with
				| TyFun(vs,rd) -> (
						(let rec f ntvs nes acc = (
							match ntvs,nes with
							| _,[] -> Left ntvs
							| [],_ -> Right(nes,acc)
							| v :: vs,e :: res -> f vs res (acc @ [e])
						) in f vs es []),rd
					)
				| _ -> raise_invalid ()
			) in
			(match rtes with
			| Left [] | Right([],_) -> TApp(ftd,es)
			| Right(res,acc) -> (
					let te1 = (TApp(e1,acc),(rd,deb)) in
					let tast,_ = fix_partial_apply (TApp(te1,res),(t,deb)) in
						tast
				)
			| Left rts -> (
					(* rtsが空でないなら、 App ftd es を、 let rec tmpf rts_1 rts_2 .. rts_n = ftd (es @ rts_1 rts_2 .. rts_n) in tmpf に変更する *)
					let tmpftd = (TyFun(rts,rd),deb) in
					let tmpf = (genvar (),tmpftd) in
					let tmpvs = List.map (fun t -> (genvar (),(t,deb))) rts in
					let te1 = TApp(ftd,es @ (List.map (fun ((_,td) as natd) -> ((TVar natd),td)) tmpvs)) in
					TLetRec(tmpf,tmpvs,(te1,tmpftd),(TVar(tmpf),tmpftd))
				)
			)
		)
	| TTuple(es) -> TTuple(mf es)
	| TLetTuple(vs,e1,e2) -> TLetTuple(vs,f e1,f e2)
	in
		(tast,(t,deb))


(*
型推論ができたなら、
単に全部 let in で結んでよいはず。
exprのみのやつは、 let _ = expr in にして。
*)







(* 
# type a = H of int;;
type a = H of int
# type b = a * int;;
type b = a * int
# type a = b * int;;
type a = b * int
# (H(3) : a);;
Error: This expression has type a/1016 but an expression was expected of type
         a/1020 = b * int
こういうシブイ話もあるが一旦無視していいよね...?
*)


type type_def = 
	| TDRename of ty
	| TDVariant of ((variant_tag * (ty list)) list)
	| TDNull

let variantenv () = [
	("@Nil",(fun x -> let t = gentype x in (0,TyUserDef("list",[t]),[])));
	("@Cons",(fun x -> let t = gentype x in (1,TyUserDef("list",[t]),[t;TyUserDef("list",[t])])));
	("Some",(fun x -> let t = gentype x in (0,TyUserDef("option",[t]),[t])));
	("None",(fun x -> let t = gentype x in (1,TyUserDef("option",[t]),[])));
]

let user_defined_types = ref [
	("int",TyInt);
	("float",TyFloat);
	("unit",TyTuple([]));
	("bool",TyInt); (*TODO(satos) さすがにいつか直す *)
	("char",TyChar);
	("string",TyStr);
]

let user_defined_type_env () = !user_defined_types

let defined_types () = [
	("list",1);
	("option",1);
] @ List.map (fun (t,_) -> (t,0)) (user_defined_type_env ())


(* 型パラメータ一はとりあえず新たな変数にしておく *)
(* TODO 型パラメータ一付きのヴァリアントが定義できるようにする *)

let eval_variant udtenv uts te = 
	let cs = ref [] in
	let na2t na = 
		try List.assoc na !cs
		with | Not_found -> (
			let t = gentype () in
			cs := (na,t) :: !cs;
			t
		)
	in
	let rec eval_variant_ udtenv te = 
		let f = eval_variant_ udtenv in
		match te with
		| ETVar na -> (
				(* TODO このへんガバ *)
				(* 実行時間はアレだが、ここで展開しないとemit時にintを展開することになる *)
				try 
					List.assoc na udtenv
				with 
					| Not_found -> 
				try 
					let _ = List.assoc na uts in TyUserDef(na,[])
				with
					| Not_found -> raise (Failure(Printf.sprintf "undefined type name %s" na))
			)
		| ETTyParam na -> na2t na
		| ETTuple ts -> TyTuple(List.map f ts)
		| ETFun(t1,t2) -> TyFun(List.map f t1,f t2)
		(* TODO ここ数があってるかチェックする *)
		| ETApp(ts,constr) -> TyUserDef(constr,(List.map f ts))
	in
		eval_variant_ udtenv te

(* 型定義に無限ループは存在しないはず 
# type a = b and b = a;;
Error: The type abbreviation a is cyclic
とか。
*)



let check_ast ast sv = 
	try (
		(* 
			f :: プログラム全体, 
			env :: 型環境, 
			venv :: tag から 型へのやつ, 
			tyenv :: 型のrenameデータ, 
			uts :: 定義済みの型集合
		*)
		
		List.fold_left (fun (f,env,venv,tyenv,uts) ast ->
			let update_by_variant na ts =                                                  (* TODO ここ若干ガバ *)
				let tts = List.map (fun (tg,te) -> (tg,List.map (fun x -> eval_variant tyenv ((na,0) :: uts) x) te)) ts in
				let ttts = List.mapi (fun i (tg,tes) -> 
					(tg,(fun _ -> (i,TyUserDef(na,[]),tes)))
				) tts
				in
				(
					f,env,
					ttts @ venv,
					tyenv,
					(na,0) :: uts
				)
			in
			let rec update_by_open filena = 
				let specs = open2spec filena in
				List.fold_left (fun (f,env,venv,tyenv,uts) spec ->
					match spec with
					| SValtype(na,te) -> (
							let rena = (String.capitalize (basename filena)) ^ "@" ^ na in
							let t = eval_variant tyenv uts te in
							let ts = schemize t env [] in (* これはなんだこれは(多分あってるけど) *)
							externs := (rena,ts) :: (!externs);
							let td = (t,default_debug_data) in
							
							if List.mem (filena ^ ".ml") stdlib_list then
								(
									f,
									(rena,ts) :: env (* TODO(satos) ここ、ちょっと不正確(わざわざopen List とかしてるとバグる) ので直す*)
									,venv,tyenv,uts
								)
							else
								(
									(fun y -> f (TLet((na,td),(TVar((rena,td)),td),y),(snd y))),
									(na,ts) :: env
									,venv,tyenv,uts
								)
						)
					| STypeRename(na,te) -> (
							let t = eval_variant tyenv uts te in
							(
								f,env,venv,
								(na,t) :: tyenv,
								(na,0) :: uts
							)
						)
					| _ -> raise (Failure("Unimplemented in open"))
				) (f,env,venv,tyenv,uts) specs
			in
			addglobalcs := [];
			match ast with
			| FExpr e -> ((
					let tast,tc,rt,_ = type_infer tyenv venv e env in
					(* print_type rt;
					print_constrs tc; *)
					let subs = unify tyenv (tc @ !addglobalcs) in
					(* print_subs subs; *)
					let rast = ast_subst subs tast in
					let sast = fix_partial_apply rast in
					(fun y -> f (TLet(("_",(rt,default_debug_data)),sast,y),(snd y)))
				),env,venv,tyenv,uts)
			| FDecl de -> (
					match de with
					| DLet(na,e) -> (
							let tast,tc,rt,_ = type_infer tyenv venv e env in
							ivprint "\ntype infer";
							ivprint (Printf.sprintf "FDecl DLet %s" na);
							vprint texp2str tast;
							ivprint "constrs";
							vprint constrs2str tc;
							let subs = unify tyenv (tc @ !addglobalcs) in
							let rast = ast_subst subs tast in
							let trt = ty_subst subs rt in
							(* print_string (texp2str rast); *)
							let trts = schemize trt env subs in
							let sast = fix_partial_apply rast in
							(
								(fun y -> f (TLet((na,(trt,default_debug_data)),sast,y),(snd y))),
								((na,trts) :: env),
								venv,tyenv,uts
							)
						)
					| DVariant(na,ts) -> update_by_variant na ts
					| DOpen(na) -> update_by_open na
					| DTypeRename(na,te) -> (
							let t = eval_variant tyenv uts te in
							(
								f,env,
								venv,
								(na,t) :: tyenv,
								(na,0) :: uts
							)
						)
				)
		) sv ast
	) with
		| TypeError(t1,t2,deb) -> 
			raise (Failure(Printf.sprintf 	
				"Type Unify Failed:\n %s \n with type %s and %s" (Debug.debug_data2str deb) (type2str t1) (type2str t2)))



let check_spec env tyenv uts specs = 
	let fn = !filename in
	let ls = String.length fn in
	let hn = (String.sub fn 0 (ls-3)) in
	let prefix = (String.capitalize hn) ^ "@" in
	
	List.fold_left (fun exports spec ->
		match spec with
		| SValtype(na,te) -> (
				let rena = prefix ^ na in
				let t = eval_variant tyenv uts te in 
				try 
					let ets = List.assoc na env in
					let et = instanciate ets in
					if is_subtype t et then
						(na,rena,t) :: exports
					else
						(print_string (Printf.sprintf "variable %s decled with type %s is not subtype of actual type %s in %s.mli\n" na (type2str t) (type2str et) hn);
						failwith (Printf.sprintf "variable %s decled with type %s is not subtype of actual type %s in %s.mli" na (type2str t) (type2str et) hn))
				with
					| Not_found -> failwith (Printf.sprintf "Undefined variable %s in %s.mli" na hn)
			)
		| STypeRename(na,te) -> (
				(*TODO verify type*)
				exports
			)
		| _ -> raise (Failure("Unimplemented in check_spec"))
	) [] specs

let exports_list = ref []

let get_exports () = !exports_list

let global_funcs () = (get_exports ()) @ (List.map fst (get_imports ()))


let check ast specs = 
	externs := [];
	exports_list := [];
	
	let sv = (
		(fun x -> x),
		(genv ()),
		(variantenv ()),
		(user_defined_type_env ()),
		(defined_types ())
	) in
	let tast,env,_,ttyenv,tuts = check_ast ast sv in
	ivprint "\ntexp without exports";
	vprint texp2str (tast (TTuple([]),(TyInt,default_debug_data)));
	let exports = check_spec env ttyenv tuts specs in
	
	exports_list := List.map (fun (_,rena,t) -> rena) exports;
	
	let te = TTuple(List.map (fun (_,rena,t) -> 
		let td = (t,default_debug_data) in
		TVar((rena,td)),td
	) exports) in
	let tt = TyTuple(List.map (fun (_,rena,t) -> t) exports) in
	let base = (te,(tt,default_debug_data)) in
	
	let tf = List.fold_left (fun ast (na,rena,t) ->
		let td = (t,default_debug_data) in
		TLet(
			(rena,td),
			(TVar((na,td)),td),
			ast
		),td
	) base exports in
		(tast tf)





let rec ty2type_expr t = 
	let self = ty2type_expr in
	match t with
	| TyInt -> ETVar("int")
	| TyFloat -> ETVar("float")
	| TyStr -> ETVar("string")
	| TyChar -> ETVar("char")
	| TyFun(ps,q) -> ETFun(List.map self ps,self q)
	| TyTuple(ts) -> ETTuple(List.map self ts)

let export_header ast = 
	externs := [];
	exports_list := [];
	
	let sv = (
		(fun x -> x),
		(genv ()),
		(variantenv ()),
		(user_defined_type_env ()),
		(defined_types ())
	) in
	let _,env,_,_,_ = check_ast ast sv in
	
	let top_names = List.concat (List.map (fun x -> 
		match x with
		| FExpr _ -> []
		| FDecl(d) -> (
			match d with
			| DLetRec(na,_,_) -> [na]
			| DLet(na,_) -> [na]
			| DOpen(na) -> [] (* TODO(satos) クリティカルなヴァリアント定義の場合はいるようになる *)
		)
	) ast) in
	let tenv = List.filter (fun (na,_) -> List.mem na top_names) env in
	List.iter (fun (na,_) -> Printf.printf "val %s\n" na) tenv;
	
	List.map (fun (na,ts) -> 
		let t = instanciate ts in
		SValtype(na,ty2type_expr t)
	) tenv 
	
	
	
	
	
	
