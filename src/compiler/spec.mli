open Syntax

type spec_decl = 
  | SValtype    of name * type_expr
  | STypeRename of name * type_expr
  | SVariant    of name * ((variant_tag * (type_expr list)) list) 
  | SOpen       of name

type top = spec_decl list

