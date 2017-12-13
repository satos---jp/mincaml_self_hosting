open Genint
open Type_checker
open Closure_conv
open Op
open Debug

let genlabel () = Printf.sprintf "@cfg_label_%d" (genint ())

let genvar (_,td) = (ref (Var (Printf.sprintf "@cfg_var_%d" (genint ()))),td)

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
		method copyset () = (
			let res = new set in
			res#addlist vs;
			res
		)
	end

type node = {
	(* 手続き型言語OCamlという気分になった *)
	mutable ops: op array; 
	src: node set;
	dst: node set;
	idx: int;
	mutable gone: bool;
}

let newnode () = { ops = [||]; src = new set; dst = new set; gone=false; idx=(-1)}

let defaultname = "@defaultname"

class cfg_type = 
	object (sl)
		val mutable vs = []
		method addv x = vs <- x :: vs
		method vs = vs

		val mutable root = newnode ()
		method setroot x = root <- x
		
		val mutable args = ([] : name list)
		method setargs x = args <- x
		method args = args
		
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
		
		(* コピー解析をする。 *)
		(* コピー、 p = ∧ pred(p) なので、おそらく最大不動点をとるのがよさそう。 *)
		method copy_anal () = (
			sl#ungone ();
			(* 各代入文に対して全探索をする *)
			let rec dfs1 v = 
				v.gone <- true;
				Array.iteri (fun p -> fun x -> (* ブロックvの命令p *)
					(* 各命令について、やっていく。 *)
					match x with
					| OpMov((na,_),(nb,_)) when !na <> !nb -> (
						let head_ok = Array.make (List.length vs) true in
						let tail_ok = Array.make (List.length vs) true in
						(* 各ブロックの先頭でコピーできうるか *)
						(* trueになるのはたかだか1回なので、O(n)でできるはず。 *)
						let visited w = not head_ok.(w.idx) || not tail_ok.(w.idx) in
						
						let rec dfs2 w =
							if visited w then () else (* 既に訪れている *)
							Array.iteri (fun q -> fun y -> 
								let gas = get_assigned y in
								if v.idx = w.idx && p = q then tail_ok.(w.idx) <- true
								else if List.mem !na gas || List.mem !nb gas then tail_ok.(w.idx) <- false 
								else ()
							) w.ops;
							
							if not tail_ok.(w.idx) then 
								w.dst#iter (fun x -> dfs2 x)
							else ()
						in
						List.iter (fun w -> if visited w then () else dfs2 w) vs;
						
						(* head_ok なものを、上から可能な限り調べていく。 *)
						List.iter (fun w -> 
							let rec loop q = 
								if q < Array.length w.ops then
									let gas = get_assigned w.ops.(q) in
									if List.mem !na gas || List.mem !nb gas then () else
									((if v.idx = w.idx && p = q then () else ()
									(*　実際に、naをnbで置換できる *)
									);
									loop (q+1))
								else ()
							in
							if head_ok.(w.idx) then loop 0 
							else if v.idx = w.idx then loop (p+1)	
							else ()
						) vs;
					)
					| _ -> ()
				) v.ops;
			in
				dfs1 root
		)
		
		
		
		method contract () = (
			vs <- List.fold_left (fun r -> fun v -> 
				(if v.dst#size = 1 && v.dst#hd.src#size = 1 then (
					let w = v.dst#hd in
					(if v = root then sl#setroot w else ());
					(* vをwにマージする。 *)
					w.ops <- Array.append v.ops w.ops;
					w.src#remove v;
					v.src#iter (fun x ->
						x.dst#remove v;
						x.dst#add w;
						w.src#add x
					);
					r
				) else  v :: r)
			) [] vs
		)
		
		method flatten_to_vlist () = (
			let res = ref [] in
			sl#idfs (fun v -> 
				res := (Array.to_list v.ops) @ !res
			);
			(* ついでに、不要なJmpを消す *)
			!res
		)
end

let addhead x v = 
	Array.append (Array.make 1 x) v
	
let addtail v x = 
	Array.append v (Array.make 1 x)

let connect_cfg csrc cdst = 
	let la = genlabel () in
	csrc.dst#add cdst;
	cdst.src#add csrc;
	csrc.ops <- addtail csrc.ops (OpJmp(la));
	cdst.ops <- addhead (OpLabel(la)) cdst.ops

(* cfgの入り口,[出口になりうるもののリスト]、を組でかえす *)
let rec to_cfgs ast tov istail cfg fn head_label = 
	let multiton ops = 
		let nd = {
			(* 雑なアドホック。 min-rt はホゲ。 *)
			ops = Array.of_list (List.filter (fun x ->
				 match x with 
				 | OpMov((x,_),_) -> !x <> (Var "@global_ret_val") 
				 | _ -> true
			) ops);
			src = new set;
			dst = new set;
			idx = List.length cfg#vs; (* ここ、O(n^2) だが、もう、ね。 *)
			gone = false;
		} in
		cfg#addv nd;
		nd
	in
	let singleton op = multiton [op]
	in
	let mres ops = 
		let v = multiton ops in v,[v]
	in
	let sres op = 
		let v = multiton [op] in v,[v]
	in
	let reccall x = 
		to_cfgs x tov istail cfg fn head_label
	in
	match ast with
	| CConst(x) -> sres (OpMovi(tov,x))
	| COp(x,vs) -> (
			match x,vs with
			| Syntax.Osemi1,[_] -> mres [] (* 虚無でよい *)
			| Syntax.Osemi2,[_;na] -> sres (OpMov(tov,cna2na na))
			| _ -> sres (OpOpr(tov,x,cvs2vs vs))
		)
	| CLet(na,e1,e2) -> (
			let na = cna2na na in
			let ch1,cts1 = (to_cfgs e1 na false cfg fn head_label) in
			let ch2,cts2 = (reccall e2) in
			List.iter (fun x -> 
				connect_cfg x ch2
			) cts1;
			(ch1,cts2)
		)
	| CIf(cmpty,a,b,e1,e2) -> (
			let a = cna2na a in
			let b = cna2na b in
			let ch1,cts1 = (reccall e1) in
			let ch2,cts2 = (reccall e2) in
			(* とりあえず、両方共Jmpにしておいて、flattenの後の最適化で消してもらう *)
			let lc1 = genlabel () in
			let lc2 = genlabel () in
			let chd = singleton (OpJcnd(cmpty,a,b,lc2)) in
			chd.ops <- addtail chd.ops (OpJmp(lc1));
			ch1.ops <- addhead (OpLabel(lc1)) ch1.ops;
			ch2.ops <- addhead (OpLabel(lc2)) ch2.ops;
			connect_cfg chd ch1;
			connect_cfg chd ch2;
			(chd,cts1 @ cts2)
		)
	| CVar(x) -> (
			if List.mem (fst x) (global_funcs ()) then 
				reccall (CClosure(x,[]))
			else 
				let x = cna2na x in
				sres (OpMov(tov,x))
		)
	| CApp(a,b) -> (
			let a = cna2na a in
			let b = cvs2vs b in
				sres (OpApp((if istail then Tail else NonTail),InDirApp,tov,a,b))
		)
	| CDirApp(a,b) -> (
			let a = cna2na a in
			let b = cvs2vs b in
			if istail && !(fst a) = (Var fn) then ( 
				(* 実際に末尾再帰させる*)
				(* 0x2b09366d9 -> 0x02ade7c115 *)
				let tvs = List.map genvar cfg#args in
				mres (
					(List.map2 (fun x -> fun y -> OpMov(x,y)) tvs b) @
					(List.map2 (fun x -> fun y -> OpMov(x,y)) (cvs2vs cfg#args) tvs) @
					[OpJmp(head_label)]
				)
			)
			else
				sres (OpApp((if istail then Tail else NonTail),DirApp,tov,a,b))
		)
	| CTuple(vs) -> (
			let vs = cvs2vs vs in
			sres (OpMakeTuple(tov,vs))
		)
	| CLetTuple(vs,ta,e1) -> (
			let vs = cvs2vs vs in
			let ta = cna2na ta in
			let ch1,cts1 = (reccall e1) in
			ch1.ops <- addhead (OpDestTuple(vs,ta)) ch1.ops;
			(ch1,cts1)
		)
	| CClosure(na,vs) -> (
			let vs = cvs2vs vs in
			sres (OpMakeCls(tov,na,vs))
		)



let cfg_toasms fn ismain args ast = 
	let ncfg = new cfg_type in
	let head_label = genlabel () in
	ncfg#setargs args;
	
	let tov = if ismain then (
		(ref (Var "@global_ret_val"),(TyInt,default_debug_data))
	) else (
		let rd = snd (snd fn) in
		let rv = ref (Var ("@ret_val_" ^ (fst fn))) in
		let rt = 
			match fst (snd fn) with
			| TyFun(_,x) -> x
			| x -> raise (Failure ("Type " ^ (type2str x) ^ " is not function type"))
		in
		(rv,(rt,rd))
	)
	in
	let retop = if ismain then [OpMainRet] else [OpRet(tov)] in
	
	let rt,gls = to_cfgs ast tov true ncfg (fst fn) head_label in
	rt.ops <- addhead (OpLabel(head_label)) rt.ops;
	ncfg#setroot rt;
	
	ncfg#contract ();
	List.iter (fun v -> 
		 v.ops <- Array.append v.ops (Array.of_list retop)
	) gls;
	let res = ncfg#flatten_to_vlist () in
	(*
	print_string (String.concat "\n" (List.map virtop2str res));
	print_newline ();
	*)
	res
	

	

