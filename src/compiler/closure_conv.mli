type name = string * (Type.ty * Debug.debug_data)
val name2str : name -> string
val vs2str : name list -> string

type cexp = 
	| CConst of Syntax.const
	| COp        of Syntax.optype * (name list)
	| CLet       of name * cexp * cexp
	| CIf        of Syntax.comptype * name * name * cexp * cexp
	| CVar       of name
	| CApp       of name * (name list)
	| CDirApp    of name * (name list)
	| CTuple     of (name list)
	| CLetTuple  of (name list) * name * cexp
	| CClosure   of name * (name list)

type globdef = {
	fn: name;
	cvs: name list;
	vs: name list;
	cbody: cexp; 
}

val clos2str : globdef list * (string list) * cexp -> string
val conv : Knorm.kexp -> globdef list * (string list) * cexp

