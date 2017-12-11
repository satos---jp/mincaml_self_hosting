open Genint
open Type_checker
open Closure_conv
open Op

let genlabel () = Printf.sprintf "@cfg_label_%d" (genint ())


class ['a] set = 
	object 
		val mutable vs = []
		val mutable size = 0
		method add (x: 'a) = vs <- x :: vs; size <- size+1
		method addlist (xs: 'a list) = vs <- xs @ vs; size <- size+(List.length xs)
		method append (xs: 'a set) = vs <- xs#vs @ vs; size <- size+xs#size
		method iter f = List.iter f vs
		method vs = vs
		method size = size
		method hd = List.hd vs
		method remove (x: 'a) = (
			vs <- List.filter (fun v -> if x = v then (size <- size-1; false) else true) vs
		)
	end

type node = {
	mutable ops: op list; 
	src: node set;
	dst: node set;
	mutable gone: bool;
	
	reach: (op ref) set;
}

let newnode () = { ops = []; src = new set; dst = new set; gone=false; reach = new set;}


class cfg_type = 
	object (sl)
		val mutable vs = []
		val mutable root = newnode ()
		method setroot x = root <- x
		method addv x = vs <- x :: vs
		method ungone () = (
			List.iter (fun x -> 
				x.gone <- false;
			) vs
		)
		
		method idfs f = (
			sl#ungone ();
			let rec rf v = 
				v.gone <- true;
				v.dst#iter (fun x -> 
					if x.gone then () 
					else rf x
				);
				f v
			in
				rf root
		)
		
		(* 到達解析をする。(この時点では、末尾最適化はしていないので、逆辺がなくて、その分を気にしなくてよい)*)
		(*
		method reach_anal () = (
			sl#ungone ();
			List.iter (fun v -> 
				v.reach#addlist (List.map (fun x -> ref x) v.ops)
			) vs;
			let rec bdfs v = 
				v.gone <- true;
				v.src#iter (fun x -> if x.gone then () else
					bdfs x;
					v.reach#append x.reach
				);
				
				v.dst#iter (fun x -> if x.gone then () else bdfs x )
			in
				bdfs root
		)
		*)
		
		method contract () = (
			vs <- List.fold_left (fun r -> fun v -> 
				if v.dst#size = 1 && v.dst#hd.src#size = 1 then (
					let w = v.dst#hd in
					(* vをwにマージする。 *)
					w.ops <- v.ops @ w.ops;
					w.src#remove v;
					v.src#iter (fun x ->
						x.dst#remove v;
						x.dst#add w;
						w.src#add x
					);
					r
				) else v :: r
			) [] vs
		)
		
		method flatten_to_vlist () = (
			let res = ref [] in
			sl#idfs (fun v -> 
				res := v.ops @ !res
			);
			(* ついでに、不要なJmpを消す *)
			!res
		)
end

let rec connect_cfg csrc cdst = 
	let la = genlabel () in
	csrc.dst#add cdst;
	cdst.src#add csrc;
	csrc.ops <- csrc.ops @ [OpJmp(la)];
	cdst.ops <- OpLabel(la) :: cdst.ops

(* cfgの入り口,[出口になりうるもののリスト]、を組でかえす *)
let rec to_cfgs ast tov istail cfg = 
	let rec singleton op = 
		let nd = newnode () in
		nd.ops <- [op];
		cfg#addv nd;
		nd
	in
	let rec sres op = 
		let v = singleton op in v,[v]
	in
	let rec reccall x = 
		to_cfgs x tov istail cfg
	in
	match ast with
	| CConst(x) -> sres (OpMovi(tov,x))
	| COp(x,vs) -> sres (OpOpr(tov,x,vs))
	| CLet(na,e1,e2) -> (
			let ch1,cts1 = (to_cfgs e1 na false cfg) in
			let ch2,cts2 = (reccall e2) in
			List.iter (fun x -> 
				connect_cfg x ch2
			) cts1;
			(ch1,cts2)
		)
	| CIf(cmpty,a,b,e1,e2) -> (
			let ch1,cts1 = (reccall e1) in
			let ch2,cts2 = (reccall e2) in
			(* とりあえず、両方共Jmpにしておいて、flattenの後の最適化で消してもらう *)
			let lc1 = genlabel () in
			let lc2 = genlabel () in
			let chd = singleton (OpJcnd(cmpty,a,b,lc2)) in
			chd.ops <- chd.ops @ [OpJmp(lc1)];
			ch1.ops <- OpLabel(lc1) :: ch1.ops;
			ch2.ops <- OpLabel(lc2) :: ch2.ops;
			connect_cfg chd ch1;
			connect_cfg chd ch2;
			(chd,cts1 @ cts2)
		)
	| CVar(x) -> (
			if List.mem (fst x) (global_funcs ()) then 
				reccall (CClosure(x,[]))
			else 
				sres (OpMov(tov,x))
		)
	| CApp(a,b) -> sres (OpApp((if istail then Tail else NonTail),InDirApp,tov,a,b))
	| CDirApp(a,b) -> sres (OpApp((if istail then Tail else NonTail),DirApp,tov,a,b))
	| CTuple(vs) -> sres (OpMakeTuple(tov,vs))
	| CLetTuple(vs,ta,e1) -> (
			let ch1,cts1 = (reccall e1) in
			ch1.ops <- OpDestTuple(vs,ta) :: ch1.ops;
			(ch1,cts1)
		)
	| CClosure(na,vs) -> sres (OpMakeCls(tov,na,vs))



let cfg_toasms ast tov istail retop = 
	let ncfg = new cfg_type in
	let rt,gls = to_cfgs ast tov istail ncfg in
	ncfg#contract ();
	ncfg#setroot rt;
	List.iter (fun v -> 
		 v.ops <- v.ops @ retop
	) gls;
	let res = ncfg#flatten_to_vlist () in
	(*
	print_string (String.concat "\n" (List.map virtop2str res));
	print_newline ();
	*)
	res
	

	

