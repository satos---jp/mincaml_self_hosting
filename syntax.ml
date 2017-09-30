open Debug

type name = string 

type const = 
	| CInt of int
	| CFloat of float
	| CBool of bool

type expr = expr_base * Debug.debug_data
and expr_base =
  | EConst of const
  | EVar       of name
  | EOp        of string * (expr list)
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | ELetRec    of name * (name list) * expr * expr
  | EApp       of expr * expr
  | ETuple     of (expr list)
  | ELetTuple  of (name list) * expr * expr
  | EArrCrt    of expr * expr
  | EArrRead   of expr * expr
  | EArrWrite  of expr * expr * expr
 
type decl = 
  | DDecl      of (name * expr) list
  | DExpr      of expr

				  
let print_name = print_string 

let err = ref (-1,-1)
let filename = ref ""

(*
let rec print_expr e =
  match e with
  | EConstInt i ->
     print_int i
  | EConstBool b ->
     print_string (string_of_bool b)
  | EVar x -> 
     print_name x
  | EAdd (e1,e2) -> 
     (print_string "EAdd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ESub (e1,e2) -> 
     (print_string "ESub (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EMul (e1,e2) -> 
     (print_string "EMul (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EDiv (e1,e2) -> 
     (print_string "EDiv (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EEq (e1,e2) ->
     (print_string "EEq (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ELt (e1, e2) ->
     (print_string "ELt (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EIf (e1,e2,e3) ->
     (print_string "EIf (";
      print_expr   e1;
      print_string ","; 
      print_expr   e2;
      print_string ",";
      print_expr   e3;
      print_string ")")
  | ELet (x,e1,e2) ->
     (print_string ("ELet (" ^ x ^ ",");
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EFun (x,e) ->
     (print_string ("EFun (" ^ x ^ ",");
      print_expr e;
      print_string ")")
  | EApp (e1,e2) ->
     (print_string "EApp (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ELetRec (id,x,e1,e2) ->
     (print_string ("ELetRec (" ^ id ^ "," ^ x ^ ",");
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
       
*)
