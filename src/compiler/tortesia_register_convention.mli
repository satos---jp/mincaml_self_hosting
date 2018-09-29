open Type
open Op

type register_convention = {
	ty2savereg : ty -> string list;
	ty2argreg  : ty -> string list;
	ty2retreg : ty -> string;
	savereg : string list;
	
	args2regs : 'a.
	(namereg list) ->
	(string -> namereg -> 'a) -> 
	(string -> namereg -> 'a) -> 	
	(int -> namereg -> 'a) -> 
	('a list)
}
val tortesia_register_convention : register_convention


