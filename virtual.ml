open Closure_conv
open Syntax
open Type_checker
open Debug
open Genint
open Op
open Cfg
open Main_option


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

let funbody2str {ops = ops; vs=vs} =
	"localval:: " ^ (String.concat " , " (List.map Closure_conv.name2str vs)) ^ "\n" ^ 
	(String.concat "\n" (List.map virtop2str ops)) ^ "\n"


let virtglobdef2str {fn=fn; vs=vs; cvs=cvs; body=bo} = 
	(name2str fn) ^ (Closure_conv.vs2str vs) ^ (Closure_conv.vs2str cvs) ^ ":\n" ^
	(funbody2str bo)

let virt2str (vgs,mfb,gvs) = 
	(String.concat "\n" (List.map virtglobdef2str vgs)) ^ "\n"^
	"globalval:: " ^ (String.concat " " gvs) ^ "\n" ^ 
	(virtglobdef2str {fn=("main",(TyInt,default_debug_data)); vs=[]; cvs=[]; body=mfb})


let rec unique_name vs = 
	match vs with
	| [] -> []
	| (x,t) :: xs -> 
		try let _ = List.assoc x xs in unique_name xs
		with
			| Not_found -> (x,t) :: (unique_name xs)

let remove_vals vs ws = List.filter (fun (x,_) -> not (List.mem x ws)) vs 

let names2str vs = "[\n" ^ (String.concat ";\n" (List.map name2str vs)) ^ ";\n]\n"


(* 命令列から、ローカルスタックに載せるべき引数を選別する *)
let get_var_names_from_ops ops remove_names = 
	let vs = List.fold_left (fun r -> fun x -> (get_var_names x) @ r) [] ops in
	remove_vals (unique_name vs) remove_names


let rec to_virtual (fundefs,globvars,rd) = 
	let globnames = (List.map (fun {fn=(x,_); cbody=_} -> x) fundefs) @ (global_funcs ()) @ globvars
	in
	(List.map (fun {fn=fn; cvs=vs1; vs=vs2; cbody=bo} -> 
		let args = vs1 @ vs2 in
		let ops = cfg_toasms fn false args bo globvars in
			{fn = fn; vs = vs1; cvs = vs2; body = {
				ops = ops;
				vs = get_var_names_from_ops ops ((List.map fst args) @ globnames);
			};}
	) fundefs),
	(let gfn = ("@global_main_func",(TyVar(-1),default_debug_data)) in
	let ops = cfg_toasms gfn true [] rd globvars in
	{
		ops = ops;
		vs = get_var_names_from_ops ops globnames;
	}),
	globvars





