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

(* ta < tb かを判定する(毎回どっちやねんとなるが) *)
(* tb　のシグネチャとして ta がありかどうか。 (int -> int ,'a -> 'a) などはあり。 *)
(* というか ta に適切な型代入をすると tb になるかですね。 *)

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


