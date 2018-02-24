open Closure_conv
open Op

type funbody = {
	ops: op list;
	vs: namereg list;
}

type virtglobdef = {
	fn: name;
	vs: namereg list;
	cvs: namereg list;
	regs: string list;
	body: funbody;
}

val to_virtual : globdef list * (string list) * cexp -> virtglobdef list * funbody * string list

val virt2str : virtglobdef list * funbody * string list -> string
