open Genint

type tyvar = int

type type_name = string

type ty =
	| TyInt
	| TyFloat
	| TyStr
	| TyChar
	| TyNum  (* int も float もこれの部分型 *)
	| TyVar of tyvar
	| TyArr of ty
	| TyFun of (ty list) * ty
	| TyTuple of ty list
	| TyUserDef of type_name * (ty list)

let tyvar2str v = Printf.sprintf "'a%d" v

let rec type2str_with_pa t =
        match t with
        | TyInt | TyFloat | TyStr | TyChar | TyVar _ | TyTuple _ | TyNum | TyUserDef _ -> type2str t
        | TyFun _ | TyArr _ -> "(" ^ (type2str t) ^ ")"

and type2str t =
	match t with
	| TyInt -> "int"
	| TyFloat -> "float"
	| TyStr -> "string"
	| TyChar -> "char"
	| TyNum -> "number"
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
				"(" ^ (List.fold_left (fun r -> fun t -> r ^ " * " ^ (type2str_with_pa t)) (type2str_with_pa x) xs) ^ ")"
		)
	| TyUserDef(s,ts) -> "UsrDef(" ^ s ^ "," ^ (String.concat " * " (List.map type2str_with_pa ts)) ^ ")"

let print_type t = print_string ((type2str t) ^ "\n")

let gentype () = TyVar(genint ())

let print_subs subs =
	print_string "[\n";
	List.iter (fun (a,b) -> 
		Printf.printf "%s => %s\n" (tyvar2str a) (type2str b);
	) subs;
	print_string "]\n"

let rec ty_var_appear t v =
        match t with
        | TyInt | TyFloat | TyNum | TyStr | TyChar -> false
        | TyFun (t1s, t2) -> List.exists (fun x -> ty_var_appear x v) (t2 :: t1s)
        | TyVar x -> x = v
        | TyArr t -> (ty_var_appear t v)
        | TyTuple ts | TyUserDef(_,ts) -> List.fold_left (fun r -> fun t -> r || (ty_var_appear t v)) false ts

let rec ty_subst subs t =
	match t with
	| TyInt | TyFloat | TyNum | TyStr | TyChar -> t
	| TyVar(nb) -> (
		try 
			let tt = (List.assoc nb subs) in 
				if List.length subs = 1 then tt else 
					ty_subst subs tt
		with
			| Not_found -> t
		)
	| TyArr x -> TyArr(ty_subst subs x )
	| TyFun(ps,q) -> TyFun(List.map (fun p -> ty_subst subs p) ps,ty_subst subs q)
	| TyTuple ps -> TyTuple(List.map (fun x -> ty_subst subs x) ps)
	| TyUserDef(na,ts) -> TyUserDef(na,List.map (fun x -> ty_subst subs x) ts)

let rec constrs_subst s cs =
        match cs with
        | [] -> []
        | (x,y,d) :: xs -> (ty_subst [s] x,ty_subst [s] y,d) :: (constrs_subst s xs)

exception TypeError of ty * ty * Debug.debug_data
(* constrs_subst のとこでO(n^2) かかっていそう。 *)
let rec unify tyenv cs = 
	(* print_constrs cs; *)
	let self = unify tyenv in
	match cs with
	| [] -> []
	| (t1,t2,deb) :: xs -> if t1 == t2 then self xs else (
		(*
		print_string ((String.concat ";" (List.map (fun (a,b) -> a) tyenv)) ^ "\n"); 
		Printf.printf "%s @ %s\n"  (type2str t1) (type2str t2);
		*)
		try 
			match t1,t2 with
			| TyVar x,y | y,TyVar x -> (
					if ty_var_appear y x then 
						raise (TypeError(t1,t2,deb)) else 
						(x,y) :: (self (constrs_subst (x,y) xs)) 
				)
			| TyArr a,TyArr b -> self ((a,b,deb) :: xs)
			| TyFun(vs,b),TyFun(ws,d) -> ( (* 部分適用に対応する。 *)
					let rec f nvs nws = (
 						match nvs,nws with
						| [],[] -> [(b,d,deb)]
						| [],rws -> [(b,TyFun(rws,d),deb)]
						| rvs,[] -> [(TyFun(rvs,b),d,deb)]
						| v :: rvs,w :: rws -> (v,w,deb) :: f rvs rws
					) in
				self ((f vs ws) @ xs)
				)
			| TyTuple ps,TyTuple qs -> self ((List.map2 (fun a b -> (a,b,deb)) ps qs) @ xs)
			(* 多相性のために追加する *)
			| TyNum,TyInt | TyInt,TyNum | TyNum,TyFloat | TyFloat,TyNum -> self xs
			| TyUserDef(a,ps),TyUserDef(b,qs) when a = b -> self ((List.map2 (fun p q -> (p,q,deb)) ps qs) @ xs)
			| TyUserDef(a,[]),b | b,TyUserDef(a,[]) when List.mem_assoc a tyenv -> (
					self ((List.assoc a tyenv,b,deb) :: xs)
				)
			| _ -> raise (TypeError(t1,t2,deb))
		with 
			| Invalid_argument("List.map2") -> raise (TypeError(t1,t2,deb))
	)


let rec get_fv t = 
	match t with
	| TyInt | TyFloat | TyNum | TyStr | TyChar -> []
	| TyFun (t1s, t2) -> List.concat (List.map get_fv (t2 :: t1s))
	| TyVar x -> [x]
	| TyArr t -> get_fv t
	| TyTuple ts | TyUserDef(_,ts) ->  List.concat (List.map get_fv ts)


type tyscheme = tyvar list * ty

let instanciate (tvs,t) = 
	let sub = List.map (fun x -> (x,TyVar(genint ()))) tvs in
	ty_subst sub t
	
let list_sub v w = 
	List.filter (fun x -> not (List.mem x w)) v

let schemize t env = 
	let env_fvs = List.concat (List.map (fun (_,(svs,s)) -> list_sub (get_fv s) svs) env) in
	let tvs = list_sub (get_fv t) env_fvs in
	(tvs,t)

let no_fv_scheme t = ([],t)

let tyscheme2str (tvs,t) = 
	(String.concat " " (List.map tyvar2str tvs)) ^ ". " ^ (type2str t)

let ty_scheme_subst subs (tvs,t) = 
	let ttvs = List.map (fun _ -> genint ()) tvs in
	let sub = List.map2 (fun x y -> (x,TyVar(y))) tvs ttvs in
	(* schemeの中身をalpha変換しておく(キャプチャ防ぐやつ) *)
	let tt = ty_subst sub t in
	(ttvs,ty_subst subs tt)

(* ta < tb かを判定する(毎回どっちやねんとなるが) *)
(* tb　のシグネチャとして ta がありかどうか。 (int -> int ,'a -> 'a) などはあり。 *)
(* というか ta に適切な型代入をすると tb になるかですね。 *)
(* TODO(satos) これ型の共変反変をミスってそうですね *)

exception Subtype_false

let rec is_sub env ta tb = 
	let self = is_sub env in
	let multi_check tas tbs = List.iter2 self tas tbs in
	if ta = tb then () else 
	match ta,tb with
	| TyVar a, tb | tb, TyVar a -> (
			try 
				let tta = List.assoc a !env in self tta tb
			with
				| Not_found -> (
						env := (a,tb) :: !env
					)
		)
	| TyNum,TyInt | TyNum,TyFloat -> ()
	| TyFun(p,q),TyFun(r,s) -> multi_check (q :: p) (s :: r)
	| TyTuple p, TyTuple r -> multi_check p r
	| TyUserDef(tag1,ps),TyUserDef(tag2,rs) when tag1 = tag2 -> multi_check ps rs
	| _ -> raise Subtype_false

let is_subtype ta tb = 
	try 
		is_sub (ref []) ta tb;
		true
	with
		| Subtype_false -> false


