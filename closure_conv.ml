open Knorm


let clos = ref []

(*
•Ê‚ÉŒ^‚ð’è‹`‚·‚é‚Ì‚ª‰­…‚É‚È‚Á‚Ä‚«‚½‚Ì‚Å
‚Æ‚è‚ ‚¦‚¸KNorm‚Å‚â‚Á‚Ä‚¢‚­B
*)

let rec remove_closure ast env = 
	match ast with
	| KLetRec(fn,args,e1,e2) -> (
			let te1 = remove_closure e1 in
			let 
			remove_closure e2
		)
	| KApp(fn,args) -> (
			
		)
	| _ -> ast

let conv x = x

