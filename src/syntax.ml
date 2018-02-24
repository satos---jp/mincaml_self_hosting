open Debug

type name = string 

type const = 
	| CInt of int
	| CFloat of float

let const2str c = 
	match c with
	| CInt x -> Printf.sprintf "CInt %d" x
	| CFloat x -> Printf.sprintf "CFloat %f" x

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

type expr = expr_base * Debug.debug_data
and expr_base =
  | EConst of const
  | EVar       of name
  | EOp        of optype * (expr list)
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | ELetRec    of name * (name list) * expr * expr
  | EApp       of expr * (expr list)
  | ETuple     of (expr list)
  | ELetTuple  of (name list) * expr * expr

type decl = 
  | DDecl      of (expr -> expr) list
  | DExpr      of expr

let print_name = print_string 

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

let expr2str ast = 
	let ss = expr2str_base ast 0 in
		String.concat "\n" (List.map (fun (d,s) -> (String.make (d*2) ' ') ^ s) ss) ^ "\n"


