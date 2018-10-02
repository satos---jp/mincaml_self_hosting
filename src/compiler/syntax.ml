open Debug


type variant_tag = string

type const = 
	| CInt of int
	| CFloat of float
	| CString of string
	| CChar of char

let const2str c = 
	match c with
	| CInt x -> Printf.sprintf "CInt %d" x
	| CFloat x -> Printf.sprintf "CFloat %f" x
	| CString x -> Printf.sprintf "CString \"%s\"" x
	| CChar x -> Printf.sprintf "CChar '%c'" x

type comptype = 
	| CmpEq
	| CmpLt

let comptype2str x = 
	match x with
	| CmpEq -> " = "
	| CmpLt -> " < "

type optype = 
	| Ominus | Oadd | Osub | Omul | Odiv
	| Ofadd | Ofsub | Ofmul | Ofdiv
	| Oeq | Oneq | Olt | Oleq | Ogt | Ogeq | Osemi1 | Osemi2 | Onot 
	| OArrCrt | OArrRead | OArrWrite
	| OSubTuple of int * int
	| OGetTuple of int
	| OGetTupleWithLen of int * int
	| Oiadd of int | Oibysub of int
	| Oimul of int | Oibydiv of int
	| OiArrRead of int | OiArrWrite of int

let op2str o = 
	match o with
	| Ominus -> "Ominus"
	| Oadd -> "Oadd"
	| Osub -> "Osub"
	| Omul -> "Omul"
	| Odiv -> "Odiv"
	| Oiadd(a) -> Printf.sprintf "Oiadd[%d]"  a
	| Oibysub(a) -> Printf.sprintf "Oibysub[%d]"  a
	| Oimul(a) -> Printf.sprintf "Oimul[%d]"  a
	| Oibydiv(a) -> Printf.sprintf "Oibydiv[%d]"  a
	| Ofadd -> "Ofadd"
	| Ofsub -> "Ofsub"
	| Ofmul -> "Ofmul"
	| Ofdiv -> "Ofdiv"
	| Oeq -> "Oeq"
	| Oneq -> "Oneq"
	| Olt -> "Olt"
	| Oleq -> "Oleq"
	| Ogt -> "Ogt"
	| Ogeq -> "Ogeq"
	| Osemi1 -> "Osemi1"
	| Osemi2 -> "Osemi2"
	| Onot -> "Onot"
	| OArrCrt -> "OArrCrt"
	| OArrRead -> "OArrRead"
	| OArrWrite -> "OArrWrite"
	| OiArrRead(a) -> Printf.sprintf "OiArrRead[%d]"  a
	| OiArrWrite(a) -> Printf.sprintf "OiArrWrite[%d]"  a
	| OSubTuple(a,b) -> Printf.sprintf "OSubTuple[%d,%d]" a b
	| OGetTuple(a) -> Printf.sprintf "OGetTuple[%d]" a
	| OGetTupleWithLen(a,b) -> Printf.sprintf "OGetTupleWithLen[%d,%d]" a b

type name = string

type pattern = 
	| PVar     of name
	| PVariant    of name
	| PVariantApp of name * pattern
	| PTuple   of (pattern list)

type expr = expr_base * Debug.debug_data
and expr_base =
  | EConst of const
  | EVar       of name
  | EOp        of optype * (expr list)
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | ELetRec    of name * (pattern list) * expr * expr
  | EApp       of expr * (expr list)
  | ETuple     of (expr list)
  | ELetTuple  of (name list) * expr * expr
  | EMatch     of expr * ((pattern * expr) list)
  | EVariant   of variant_tag * expr list

type type_expr = 
	| ETVar     of name (* ふつうに、 int とか list とか *)
	| ETTyParam  of name (* 'a とかの、型多相のためのやつ *)
	| ETTuple   of type_expr list
	| ETFun   of (type_expr list) * type_expr (* 部分適用できないのでクリティカル *)
	| ETApp   of (type_expr list) * name

let rec type_expr2header te = 
	let self = type_expr2header in
	match te with
	| ETVar na -> na
	| ETFun(ts,t) -> "(" ^ (String.concat " -> " (List.map self (ts @ [t]))) ^ ")"
	| ETTuple([]) -> "unit"
	| ETTuple(ts) -> "(" ^ (String.concat " * " (List.map self ts)) ^ ")"

type decl = 
  | DLet        of name * expr
  | DLetRec     of name * (name list) * expr
  | DTypeRename of name * type_expr
  | DVariant    of name * ((variant_tag * (type_expr list)) list)
  | DOpen       of name

type decl_expr = 
	| FExpr of expr
	| FDecl of decl

type top = decl_expr list


let print_name = print_string 


let rec pat2str_base pat d = 
	match pat with
	| PVar na -> [(d,"PVar " ^ na)]
	| PVariant    na -> [(d,"PVariant " ^ na)]
	| PVariantApp(na,pa) -> (d,"PVariantApp " ^ na) :: pat2str_base pa (d+2)
	| PTuple(ps) -> (d,"(") :: List.concat (List.map (fun x -> pat2str_base x (d+2)) ps) @ [(d,")")]

let rec expr2str_base astdeb d = 
	let ast,_ = astdeb in
	match ast with
	| EConst x -> [(d,const2str x)]
	| EVar(x) -> [(d,"Var " ^ x)]
	| EOp(op,es) -> (d,op2str op) :: List.concat (List.map (fun x -> (expr2str_base x (d+1))) es)
	| EIf(e1,e2,e3) -> (
			(d,"If") :: (expr2str_base e1 (d+1))
				@ [(d,"Then")] @ (expr2str_base e2 (d+1)) @ [(d,"Else")] @ (expr2str_base e3 (d+1))
		)
	| ELet(na,e1,e2) -> (d,"Let " ^ na ^ " =") :: (expr2str_base e1 (d+1)) @ [(d,"In")] @ (expr2str_base e2 (d+1))
	| ELetRec(na,vs,e1,e2) -> (d,"LetRec " ^ na ^ " =") :: (expr2str_base e1 (d+1)) @ [(d,"In")] @ (expr2str_base e2 (d+1))
	| EApp(e1,es) -> (d,"App ") :: (expr2str_base e1 (d+1)) @ List.concat (List.map (fun x -> (expr2str_base x (d+2))) es)
	| ETuple(es) -> (d,"( ") :: List.concat (List.map (fun x -> (expr2str_base x (d+1))) es) @ [(d," )")]
	| ELetTuple(vs,e1,e2) -> (d,"Let " ^ (String.concat " , " vs) ^ " = ") :: (expr2str_base e1 (d+1)) @ [(d,"In")] @ (expr2str_base e2 (d+1))
	| EMatch(e,pes) -> (d,"match") :: (expr2str_base e (d+2)) @ [(d,"with")] @ (
			List.concat (List.map (fun x -> (d+1,"|") :: (pat2str_base (fst x) (d+2))) pes)
		)
	| EVariant(tag,es) -> (d,Printf.sprintf "Variant %s" tag) :: List.concat (List.map (fun x -> (expr2str_base x (d+2))) es)
 
let expr2str ast = 
	let ss = expr2str_base ast 0 in
		String.concat "\n" (List.map (fun (d,s) -> (String.make (d*2) ' ') ^ s) ss) ^ "\n"



let decl2str de = 
	let d = 0 in
	match de with
	| DLet(na,e) -> (d,"DLet " ^ na ^ " =") :: (expr2str_base e (d+1))
	| DLetRec(na,vs,e) -> (d,"DLetRec " ^ na ^ " =") :: (expr2str_base e (d+1))
	| DVariant(na,tts) -> (d,"DVariant " ^ na ^ " =") :: (
			List.concat (List.map (fun (tag,ts) -> (d+1,"| " ^ tag ^ " of ") :: (
				[(d+2,Printf.sprintf "len of %d" (List.length ts))]
			)) tts)
		)
	| DOpen(na) -> [(d,"Open " ^ na)]
	| DTypeRename (na,te) -> [(d,"DTypeRename " ^ na ^ " =" ^ " hogemitainayatu")]


let decl_expr2str ast = 
	match ast with
	| FExpr e -> (0,"[\n") :: (expr2str_base e 2) @ [(0,"\n]\n")]
	| FDecl d -> decl2str d

let top2str asts =
	let ss = List.concat (List.map decl_expr2str asts) in
		String.concat "\n" (List.map (fun (d,s) -> (String.make (d*2) ' ') ^ s) ss) ^ "\n"






