open Syntax
open Source2ast

type spec_decl = 
  | SValtype    of name * type_expr
  | STypeRename of name * type_expr
  | SVariant    of name * ((variant_tag * (type_expr list)) list) 
  | SOpen       of name 

type top = spec_decl list

(*
let check_type_definition tyenv specs = 
	List.fold_left (fun env spc -> 
		match spc with
		| SOpen na -> (
				let te = Source2ast.mli2spec (na ^ ".ml") in
				env
			)
	) [] specs
	これは先でいいやろ
*)


let top2header vs opens = 
	(String.concat "" (List.map (fun x -> 
		Printf.sprintf "Open %s\n" x
	) opens)) ^ 
	(String.concat "" (List.map (fun x -> 
		match x with
		| SValtype(na,te) -> (
				let cf = (try let _ = String.index na '@' in (fun s -> "(* " ^ s ^ " *)") with Not_found -> (fun s -> s)) in
				(cf (Printf.sprintf "val %s : %s" na (type_expr2header te))) ^ "\n"
			)
	) vs))

let spec_open_list = ref []

let implicit_open s = 
	if List.mem s !spec_open_list then () else 
	spec_open_list := s :: !spec_open_list


