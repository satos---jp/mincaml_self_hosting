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
	vs: namereg list;
}

type virtglobdef = {
	fn: name;
	vs: namereg list;
	cvs: namereg list;
	regs: string list;
	body: funbody;
}

let funbody2str {ops = ops; vs=vs} =
	"localval:: " ^ (vs2str vs) ^ "\n" ^ 
	(String.concat "\n" (List.map virtop2str ops)) ^ "\n"

let virtglobdef2str {fn=fn; regs=regs; vs=vs; cvs=cvs; body=bo} = 
	(name2str fn) ^ (vs2str vs) ^ (vs2str cvs) ^ ":\n" ^
	"Used registers: " ^ (String.concat " " regs) ^ "\n" ^
	(funbody2str bo)

let virt2str (vgs,mfb,gvs) = 
	(String.concat "\n" (List.map virtglobdef2str vgs)) ^ "\n"^
	"globalval:: " ^ (String.concat " " gvs) ^ "\n" ^ 
	(virtglobdef2str {fn=("main",(TyInt,default_debug_data)); vs=[]; regs=[]; cvs=[]; body=mfb})


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
	let vs = List.fold_left (fun r -> fun x -> (get_var_nameregs x) @ r) [] ops in
	remove_vals (unique_name vs) remove_names


let rec to_virtual (fundefs,heapvars,rd) = 
	let funnames = (List.map (fun {fn=(x,_); cbody=_} -> x) fundefs) @ (global_funcs ())
	in
	(List.map (fun {fn=fn; vs=vs1; cvs=vs2; cbody=bo} -> 
		let args = vs1 @ vs2 in
		let ops,regs,cvs2vs = cfg_toasms fn false args bo funnames heapvars in
			{fn = fn; vs = (cvs2vs vs1); regs = regs; cvs = (cvs2vs vs2); body = {
				ops = ops;
				vs = get_var_names_from_ops ops (List.map fst (cvs2vs args));
			};}
	) fundefs),
	(let gfn = ("@global_main_func",(TyVar(-1),default_debug_data)) in
	let ops,_,_ = cfg_toasms gfn true [] rd funnames heapvars in
	{
		ops = ops;
		vs = get_var_names_from_ops ops [];
	}),
	heapvars





