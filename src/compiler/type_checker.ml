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
	("raise_match_failure",TyFun([gentype ()],gentype ()));
	("failwith",TyFun([TyStr],gentype ()));

	("ref",let t = gentype() in TyFun([t],TyRef(t)));
	("@ref@get",let t = gentype() in TyFun([TyRef(t)],t));
	("@ref@set",let t = gentype() in TyFun([TyRef(t);t],TyTuple([])));
] @ [ (* lib_string.s *)

	("String@@",TyFun([TyStr;TyStr],TyStr));
	("String@length",TyFun([TyStr],TyInt));
	("String@get",TyFun([TyStr;TyInt],TyChar));

] @ [ (* char.s *)

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
								TOp(OGetTuple(1),[pna]),(TyTuple([nt]),deb)
							]),(nt,deb)))
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
						(TIf(rcond,(TLet(na,(TOp(OGetTuple(i),[pna]),(nt,deb)),cond),(TyInt,deb)),false_expr),(TyInt,deb)),
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
	| (x,y,d) :: xs -> (
			(Printf.sprintf "(%s , %s) : %s\n" (type2str x) (type2str y) (debug_data2str d)) ^
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



let get_format_string_type s = 
	 Some (fun t -> TyFun([TyStr],t))


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
	| EConst(CString x) -> (
			(TConst(CString x),[],TyStr (*
				(match get_format_string_type x with 
				| Some f -> let v = gentype () in TyUserDef("format",[f v;v])
				| None -> TyStr) *)
			)
		)
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

			ivprint ("letdecl " ^ n1 ^ "as scheme" ^ (tyscheme2str ts2) ^ "\n");
			ivprint (constrs2str c2);
			
			let te3,c3,tt3,_ = self e3 ((n1,ts2) :: env) in
			
			(* TODO(satos) デバッグ用の名前も、多分schemaにしないといけないんだけど面倒なので飛ばす *)
			(* これc2も外に出さなきゃいけないよね...??(毎回言ってんな...)(c2のうち、不要なのは消しといたほうがよさそう...(計算量的に)) *)
			(* これ正しくはenvもアップデートされますねここで!?(設計がやばくなりそう)*)
			(TLet((n1,(ttt2,deb2)),tte2,te3),c2 @ c3,tt3)
		)
	| ELetRec([f,ps,e2],e3) -> (
			let ns = List.map (fun x -> match x with | PVar v -> v | _ -> raise (Failure "Shouldn't reach here in type_checker ELetRec")) ps in
			
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
			とゆーか微妙にバグってるね？ 
			*)
			
			let btns = List.map (fun x -> (x,gentype ())) ns in 
			let tns = List.map (fun (a,b) -> (a,no_fv_scheme b)) btns in
			
			let te2,c2,tt2,_ = self e2 (tns @ env) in
				(TLetTuple(List.map (fun (x,t) -> (x,(t,deb1))) btns,te1,te2),
				(tt1,TyTuple(List.map (fun (_,x) -> x) btns),deb1) :: c1 @ c2,tt2)
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
			let tet =  List.map (fun ([pat],e) ->  
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







let rec forall_samelen f v w = 
	let self = forall_samelen f in
	match v,w with
	| [],[] -> true
	| (x :: xs),(y :: ys) -> (f x y) && (self xs ys)
	| _ -> false

(* 型の"型"が同じかどうか *)
let rec same_type_type a b = 
	let self = same_type_type in
	match a,b with
	| TyArr(at),TyArr(bt) | TyRef(at),TyRef(bt) -> self at bt
	| TyFun(ats,at),TyFun(bts,bt) -> (self at bt) && (forall_samelen self ats bts)
	| TyTuple(ats),TyTuple(bts) -> (forall_samelen self ats bts)
	| TyUserDef(_,ats),TyUserDef(_,bts) -> (forall_samelen self ats bts)
	| _ -> true

(* 
	部分適用を解消するべき点は2つある(今のとこ)
	・ f x1 ... xn で と fの型と個数があっていないとき
	・ 一種のキャストが入るとき (たとえば、 引数 xk での型があってないとき) (型の等式制約解消時に発生しうる)
*) 



type ('a, 'b) either = Left of 'a | Right of 'b

(* この関数の返り値のtは変更しない。 *)
let rec fix_partial_apply (ast,(t,deb)) =  
	let f = fix_partial_apply in
	let mf = List.map f in
	let tast = match ast with
	| TConst _ | TVar _ -> ast
	| TOp(op,es) -> TOp(op,mf es)
	| TIf(e1,e2,e3) -> TIf(f e1,f e2,f e3)
	| TLet(na,e1,e2) -> TLet(na,f e1,f e2)
	| TLetRec(na,vs,e1,e2) -> TLetRec(na,vs,f e1,f e2)
	| TTuple(es) -> TTuple(mf es)
	| TLetTuple(vs,e1,e2) -> TLetTuple(vs,f e1,f e2)
	| TApp(e1,e2) -> (
			let raise_invalid () = 
					raise (Failure(Printf.sprintf "Invalid apply for %s with (%s) at %s" 
							(type2str t) (String.concat ", " (List.map (fun (_,(t,_)) -> (type2str t)) e2)) (debug_data2str deb)
					))
			in
			(* t1 に vt1 ... vtn を適用した型と t の型が一致するように t1 の型を変更する ので、 t1 は vt1 -> vt2 -> ... vtn -> t になるはず *)
			let es = mf e2 in
			let est = TyFun(List.map (fun (_,(t,_)) -> t) es,t) in
			let te1 = cast_to raise_invalid (f e1) est in
				TApp(te1,es)
		)
	in
		(tast,(t,deb))


		
(* ft 型のastを tt型 にする*)
(* TyFunの引数分だけが違っているはず *)
and cast_to raisefun ((ast,(ft,deb)) as astdeb) tt = 
	let self = cast_to raisefun in
	let rec ts2ns = 
		function 
		| [] -> []
		| t :: xs -> (genvar (),(t,deb)) :: (ts2ns xs)
	in
	let n2ve ((_,td) as na) = (TVar(na),td) in
	if same_type_type ft tt then astdeb else
	match ft,tt with
	| TyFun(ats,at),TyFun(bts,bt) when (List.length ats) = (List.length bts) -> (
			(*
			print_string ("samelen ast :: " ^ (texp2str astdeb) ^ "\n");
			*)
			(* 逆η簡約する. let tf xs' = f xs in tf. *)
			let xsp = ts2ns bts in
			let xs = List.map2 self (List.map n2ve xsp) ats in
			
			let tftd = tt,deb in
			let tfn= (genvar (),tftd) in
			TLetRec(tfn,xsp,
				(self (TApp(astdeb,xs),(at,deb)) bt),
				(n2ve tfn)
			),(tt,deb)
		)
	| TyFun(ats,at),TyFun(bts,bt) -> (
		(*
		print_string ("difflen ast :: " ^ (texp2str astdeb));
		print_string ("totype :: " ^ (type2str tt) ^ "\n\n");
		*)
		let lats = List.length ats in
		let lbts = List.length bts in
		if lats > lbts then (
			(* f: A -> B -> C を A -> (B -> C) にするには、 let tf x = (let ttf y = f x y in ttf) in tf *)
			let tys,txs = 
				let rec g = 
					function
					| ps,[] -> ps,[]
					| p :: pxs,_ :: qxs -> let rs,ss = g (pxs,qxs) in rs,(p :: ss)
				in
					g (ats,bts)
			in
			let xs,ys = ts2ns txs,ts2ns tys in
			let tftd = (TyFun(txs,TyFun(tys,at)),deb) in
			let ttftd = (TyFun(tys,at),deb) in
			let tfn= (genvar (),tftd) in
			let ttfn = (genvar (),ttftd) in
			let tast = TLetRec(tfn,xs,
				(TLetRec(ttfn,ys,
					(TApp(astdeb,List.map n2ve (xs @ ys)),(at,deb)),
					(n2ve ttfn)),ttftd),
				(n2ve tfn)
			),tftd in
			self tast tt
		) else (
			(* f: A -> (B -> C) を A -> B -> C にするには、 let tf x' y' = (f x)' y' in tf *)
			let tys,txs = 
				let rec g = 
					function
					| [],qs -> qs,[]
					| _ :: pxs,q :: qxs -> let rs,ss = g (pxs,qxs) in rs,(q :: ss)
				in
					g (ats,bts)
			in
			let xsp,ysp = ts2ns txs,ts2ns tys in
			let tftd = tt,deb in
			let tfn= (genvar (),tftd) in
			TLetRec(tfn,xsp @ ysp,
				(TApp(
					(self (TApp(
						astdeb,
						List.map2 self (List.map n2ve xsp) ats
					),(at,deb)) (TyFun(tys,bt))),
					(List.map n2ve ysp)
				),(bt,deb)),
				(n2ve tfn)
			),tftd
			(*
			let d = lbts - lats in
			let advs = List.map (fun t -> (genvar (),t)) dts in
			let tft = TyFun(
			let tfn = (genvar (),tft) in
			let fx = TApp(astdeb,xs), in
			TLetRec(tfn,
				TApp(ast,v1s),
			((TLet(tfn,TLetRec(tf,tmpvs,(te1,tmpftd),(TVar(tmpf),tmpftd)),(t,deb))
			
			
				let te1 = (TApp(e1,acc),(rd,deb)) in
				let tast,_ = fix_partial_apply (TApp(te1,res),(t,deb)) in
					tast
			)
			*)
		)
	)
	| TyUserDef(atn,ats),TyUserDef(btn,bts) -> (
			assert (atn = btn);
			let tagn = (genvar (),(TyInt,deb)) in
			let etagn = TVar(tagn),(TyInt,deb) in
			let bodn = (genvar (),(TyTuple(ats),deb)) in
			let ebodn = TVar(bodn),(TyTuple(ats),deb) in
			TLet(tagn,
				(TOp(OGetTuple(0),[astdeb]),(TyInt,deb)),
			(TLet(bodn,
				(TOp(OGetTuple(1),[astdeb]),(TyTuple(ats),deb)),
				(TTuple([etagn; self ebodn (TyTuple(bts))]),(tt,deb))
			),(tt,deb))),(tt,deb)
		)
	| TyTuple(ats),TyTuple(bts) -> (
			assert (List.length ats = List.length bts); 
			
			let tmpvs = List.map (fun t -> (genvar (),(t,deb))) ats in  
			List.fold_left (fun r (i,(v,td)) -> 
				TLet((v,td),(TOp(OGetTuple(i),[astdeb]),td),r),(tt,deb)
			) (
				TTuple(List.map2 (fun (v,td) ntt -> self (TVar(v,td),td) ntt) tmpvs bts),(tt,deb)
			) (List.mapi (fun i d -> (i,d)) tmpvs)
		)
	| _ -> failwith (Printf.printf "cast from %s to %s is not implemented yet" (type2str ft) (type2str tt); "") 
			
	(*
	| TyFun(vs,rd),TyFun(es,_) -> (
			(let rec f ntvs nes acc = (
			match ntvs,nes with
			| _,[] -> Left ntvs
			| [],_ -> Right(nes,acc)
			| v :: vs,e :: res -> f vs res (acc @ [e])
			) in f vs es []),rd
		
		| _ -> astdeb (* TODO(satos) ここ、型について再帰的にしないといけない気がしますね... *)
	in
	(* Left  vs     は e1 の型が余る(部分適用) *)
	(* Right es,acc は e1 の型が足りなくなる(過剰適用) *)
	(match rtes with
	| Left [] | Right([],_) -> (
			(* 引数側のキャストを解決する *)
			(* f: A -> (B -> C) を A -> B -> C にするには、 let tf x y = (f x) y in tf *)
			(* f: A -> B -> C を A -> (B -> C) にするには、 let tf x = (let ttf y = f x y in ttf) in tf *)
			TApp(ftd,es)
		)
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
			let tast,_ = fix_partial_apply (TLetRec(tmpf,tmpvs,(te1,tmpftd),(TVar(tmpf),tmpftd)),(t,deb)) in
				tast
		)
	*)


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

let rec range n f = 
	if n < 0 then [] else (f n) :: (range (n-1) f) 

let variantenv () = [
	("@Nil",(fun x -> let t = gentype x in (0,TyUserDef("list",[t]),[])));
	("@Cons",(fun x -> let t = gentype x in (1,TyUserDef("list",[t]),[t;TyUserDef("list",[t])])));
	("Some",(fun x -> let t = gentype x in (0,TyUserDef("option",[t]),[t])));
	("None",(fun x -> let t = gentype x in (1,TyUserDef("option",[t]),[])));
	("Token",(fun x -> let t = gentype x in (0,TyUserDef("parsing_data",[t]),[t])));
	("Datum",(fun x -> let t = gentype x in (1,TyUserDef("parsing_data",[t]),[TyInt;TyInt;TyUserDef("list",[TyUserDef("parsing_data",[t])])])));
] @ (
	(* let rec - and - のための *)
	(range 30 (fun i -> (Printf.sprintf "@Func%d" i,(fun x -> (i,TyUserDef("@lra_functions",[]),[]))))) @
	(range 30 (fun i -> (Printf.sprintf "@Retval%d" i,(fun x -> let ts = range 30 (fun _ -> gentype x) in (i,TyUserDef("@lra_retvals",ts),[List.nth ts i])))))
)

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
	("ref",1);
	("parsing_data",1);
	("format",2); (* TODO(satos) OCamlに合わせるなら3. *)
	(* let rec - and - のための *)
	("@lra_functions",0);
	("@lra_retvals",30);
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



let check_ast nowna ast sv opens = 
	try (
		(* 
			f :: プログラム全体, 
			env :: 型環境, 
			venv :: tag から 型へのやつ, 
			tyenv :: 型のrenameデータ, 
			uts :: 定義済みの型集合
		*)

		let rec update_by_open sv filena isexplicit = (* open Hoge は explicit open, Hoge.huga は implicit open *)
			Printf.printf "update_by_open %s\n" filena;
			let specs,opens = open2spec filena in
			let tsv = List.fold_left (fun r fn -> update_by_open r fn false) sv opens in
			(*
			Printf.printf "update_by_open_tsv %s\n" filena;
			let (_,_,_,_,uts) = tsv in
			List.iter (fun (s,i) -> Printf.printf "uts : %s : %s,%d\n" filena s i) uts;
			*)
			List.fold_left (fun (f,env,venv,tyenv,uts) spec ->
				let update_by_variant na ts =                                                  (* TODO ここ若干ガバ *)
					let tts = List.map (fun (tg,te) -> (tg,List.map (fun x -> eval_variant tyenv ((na,0) :: uts) x) te)) ts in
					let ttts = List.mapi (fun i (tg,tes) -> 
						(tg,(fun _ -> (i,TyUserDef(na,[]),tes)))
					) tts
					in
						if isexplicit then 
							(
								f,env,
								ttts @ venv,
								tyenv,
								(na,0) :: uts
							)
						else
							let rena = (String.capitalize (basename filena)) ^ "@" ^ na in
							( (* TODO(satos) ここもガバですね(あとで直す)*)
								f,env,
								(List.map (fun (s,v) -> ((String.capitalize (basename filena)) ^ "@" ^ s),v) ttts) @ venv,
								tyenv,
								(na,0) :: (rena,0) :: uts
							)
				in
				match spec with
				| SValtype(na,te) -> (
						let rena = (String.capitalize (basename filena)) ^ "@" ^ na in
						let t = eval_variant tyenv uts te in
						let ts = schemize t env [] in (* TODO(satos) これはなんだこれは(多分あってるけど) *)
						(* TODO(satos) ここ、.s と .ml 両方あるstringのための応急処置 *)
						(if filena = "String" && nowna = (!path_to_library ^ "/ml/string.ml") then () else externs := (rena,ts) :: (!externs));
						let td = (t,default_debug_data) in
						
						if isexplicit then
							(
								(fun y -> f (TLet((na,td),(TVar((rena,td)),td),y),(snd y))),
								(na,ts) :: env
								,venv,tyenv,uts
							)
						else
							(
								f,
								(rena,ts) :: env 
								,venv,tyenv,uts
							)
					)
				| STypeRename(na,te) -> (
						let t = eval_variant tyenv uts te in
						let rena = (String.capitalize (basename filena)) ^ "@" ^ na in
						if isexplicit then 
							(
								f,env,venv,
								(na,t) :: tyenv,
								(na,0) :: uts
							)
						else
							( (* TODO(satos) ここもガバですね(あとで直す)*)
								f,env,venv,
								(na,t) :: (rena,t) :: tyenv,
								(na,0) :: (rena,0) :: uts
							)
					)
				| SVariant(na,ts) -> update_by_variant na ts
				| SOpen(na) -> ( (* 必要なのは型のrename情報だけなはずなので、それのみをimportする*)
						let (_,_,tvenv,ttyenv,tuts) = update_by_open (f,env,venv,tyenv,uts) na true in
						(f,env,tvenv,ttyenv,tuts)
					)
				| _ -> raise (Failure("Unimplemented in open"))
			) tsv specs
		in
		let tsv = List.fold_left (fun r fn -> update_by_open r fn false) sv opens in
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
			addglobalcs := [];
			match ast with
			| FExpr e -> ((
					let tast,tc,rt,_ = type_infer tyenv venv e env in
					(* print_type rt;
					print_constrs tc; *)
					let subs = unify tyenv (tc @ !addglobalcs) in
					(* print_subs subs; *)
					let rast = ast_subst subs tast in
					(* print_string ("Before fix partial apply" ^ (texp2str tast)); *)
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
							(* print_string ("Before fix partial apply" ^ (texp2str rast)); *)
							let trts = schemize trt env subs in
							let sast = fix_partial_apply rast in
							(
								(fun y -> f (TLet((na,(trt,default_debug_data)),sast,y),(snd y))),
								((na,trts) :: env),
								venv,tyenv,uts
							)
						)
					| DVariant(na,ts) -> update_by_variant na ts
					| DOpen(na) -> update_by_open (f,env,venv,tyenv,uts) na true
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
		) tsv ast
	) with
		| TypeError(t1,t2,deb) -> 
			raise (Failure(Printf.sprintf 	
				"Type Unify Failed:\n %s \n with type %s and %s" (Debug.debug_data2str deb) (type2str t1) (type2str t2)))



let check_spec fn env tyenv uts specs = 
	let fn = basename fn in
	let ls = String.length fn in
	let hn = (String.sub fn 0 (ls-3)) in
	let prefix = (String.capitalize hn) ^ "@" in
	
	List.fold_left (fun exports spec ->
		match spec with
		| SValtype(na,te) -> (
				let rena = prefix ^ na in
				let t = eval_variant tyenv uts te in 
				Printf.printf "export %s with type %s\n" na (type2str t);
				try 
					let ets = List.assoc na env in
					let et = instanciate ets in
					if is_subtype t et then
						(na,rena,t) :: exports
					else
						(print_string (Printf.sprintf "variable %s decled with type %s is not subtype of actual type %s in %s.mli\n" na (type2str t) (type2str et) hn);
						failwith (Printf.sprintf "variable %s decled with type %s is not subtype of actual type %s in %s.mli" na (type2str t) (type2str et) hn))
				with
					| Not_found -> failwith (Printf.sprintf "Undefined variable %s in %s.ml required in .mli" na hn)
			)
		| STypeRename(na,te) -> (
				(*TODO verify type*)
				exports
			)
		| SVariant(na,ts) -> (
				(*TODO verify variant*)
				exports
			)
		| SOpen(na) -> (
				(*TODO verify open*)
				exports
			)
		| _ -> raise (Failure("Unimplemented in check_spec"))
	) [] specs

let exports_list = ref []

let get_exports () = !exports_list

let global_funcs () = (get_exports ()) @ (List.map fst (get_imports ()))


let check fn ast specs opens = 
	externs := [];
	exports_list := [];
	let sv = (
		(fun x -> x),
		(genv ()),
		(variantenv ()),
		(user_defined_type_env ()),
		(defined_types ())
	) in
	let tast,env,_,ttyenv,tuts = check_ast fn ast sv opens in
	ivprint "\ntexp without exports";
	vprint texp2str (tast (TTuple([]),(TyInt,default_debug_data)));
	let exports = check_spec fn env ttyenv tuts specs in
	
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
	| TyUserDef(s,ts) -> ETApp(List.map self ts,s)
	| TyVar v  -> ETVar(tyvar2str v)

let export_header fn ast opens = 
	externs := [];
	exports_list := [];
	
	let sv = (
		(fun x -> x),
		(genv ()),
		(variantenv ()),
		(user_defined_type_env ()),
		(defined_types ())
	) in
	let _,env,_,_,_ = check_ast fn ast sv opens in (* TODO(satos) ここ、必要な分はheaderでopenさせるようにする *)
	
	let top_names = List.concat (List.map (fun x -> 
		match x with
		| FExpr _ -> []
		| FDecl(d) -> (
			match d with
			| DLet(na,_) -> [na]
			| DOpen(na) -> [] (* TODO(satos) クリティカルなヴァリアント定義の場合はいるようになる *)
			| DTypeRename(na,te) -> [] (* TODO(satos) あとでする *)
		)
	) ast) in
	let tenv = List.filter (fun (na,_) -> List.mem na top_names) env in
	List.iter (fun (na,_) -> Printf.printf "val %s\n" na) tenv;
	
	List.map (fun (na,ts) -> 
		let t = instanciate ts in
		SValtype(na,ty2type_expr t)
	) tenv 
	
	
	
	
	
	
