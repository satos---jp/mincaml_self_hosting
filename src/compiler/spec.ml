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
