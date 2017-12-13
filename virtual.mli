open Closure_conv
open Op

type funbody = {
	ops: op list;
	vs: name list;
}

type virtglobdef = {
	fn: name;
	vs: name list;
	cvs: name list;
	body: funbody;
}

val to_virtual : (Closure_conv.name * globdef) list * (string list) * cexp -> virtglobdef list * funbody * string list

val virt2str : virtglobdef list * funbody * string list -> string
