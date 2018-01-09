open Genint
open Type_checker
open Closure_conv
open Op
open Debug
open Main_option

let genlabel () = Printf.sprintf "@cfg_label_%d" (genint ())

let genvar (_,td) = (Printf.sprintf "@cfg_var_%d" (genint ()),td)

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
	mutable idx: int;
	mutable gone: bool;
}

let newnode () = { ops = [||]; src = new set; dst = new set; gone=false; idx=(-1)}

let node2str { ops=ops; src=src; dst=dst; idx=idx; gone=gone; } = 
	(Printf.sprintf "idx = %d\n" idx) ^ 
	"ops {\n\t" ^ 
	(String.concat "\n\t" (List.map virtop2str (Array.to_list ops))) ^ "\n" ^
	"}\n" ^ 
	"src = [" ^ (String.concat " , " (List.map (fun x -> (Printf.sprintf "%d" x.idx)) src#vs)) ^ "]\n" ^
	"dst = [" ^ (String.concat " , " (List.map (fun x -> (Printf.sprintf "%d" x.idx)) dst#vs)) ^ "]\n"


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
		
		method dump_cfg () = (
			Printf.printf "root %d\n" root.idx;
			List.iter (fun x -> 
				Printf.printf "%s\n" (node2str x)
			) vs
		) 
		
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
			(*
			Printf.printf "start_copy_anal\n";
			sl#dump_cfg ();
			*)
			
			sl#ungone ();
			(* 各代入文に対して全探索をする *)
			let rec dfs1 v = 
				v.gone <- true;
				Array.iteri (fun p -> fun x -> (* ブロックvの命令p *)
					(* 各命令について、やっていく。 *)
					match x with
					| OpMov((na,_),(nb,_)) when !na <> !nb -> (
						(* 引数まわりから入ってくる変数はあきらめる *)
						let cut_at_root = 
							let tas = List.map (fun (x,_) -> Var x) args in
							(List.mem !na tas || List.mem !nb tas)
						in
						(*
						Printf.printf "idx %d th %d :: %s cutat %d is %b\n" v.idx p (virtop2str x) root.idx cut_at_root;
						*)
						let head_ok = Array.make (List.length vs) true in
						let tail_ok = Array.make (List.length vs) true in
						(* 各ブロックの先頭でコピーできうるか *)
						(* trueになるのはたかだか1回なので、O(n)でできるはず。 *)
						let visited w = not head_ok.(w.idx) || not tail_ok.(w.idx) in
						
						let rec dfs2 w b =
							if visited w then () else ( (* 既に訪れている *)
							(if not b || (cut_at_root && w.idx = root.idx) then ( (* だめという文脈で訪れた *)
								(*
								Printf.printf "dame at %d\n" w.idx;
								*)
								head_ok.(w.idx) <- false;
								tail_ok.(w.idx) <- false
							) else ());
							
							Array.iteri (fun q -> fun y -> 
								let gas = get_assigned y in
								if v.idx = w.idx && p = q then tail_ok.(w.idx) <- true
								else if List.mem !na gas || List.mem !nb gas then tail_ok.(w.idx) <- false 
								else ()
							) w.ops;
							(*
							Printf.printf "%s\n" (node2str w);
							*)
							if not tail_ok.(w.idx) then 
								w.dst#iter (fun x -> 
									dfs2 x false;
									head_ok.(x.idx) <- false
								)
							else ())
						in
						List.iter (fun w -> dfs2 w true) vs;
						
						(* 置換できそうなものを、上から可能な限り調べていく。 *)
						List.iter (fun w -> 
							let rec loop q = 
								if q < Array.length w.ops then
									let nop = w.ops.(q) in
									if v.idx = w.idx && p = q then () (* 自分自身は置換しない *)
									else (
										(*　実際に、nbをnaで置換できる *)
										(*  0x0000000237901d1a -> 0x0000000231ed9750 *)
										(* TODO 多分、mov eax,eax的なのをこの時点で除いたほうが置換が進む *)
										(*
											Printf.printf "really subst %d : %s \n" q (virtop2str nop);
										*)
										let b = ref false in
										let gan = get_assigner nop in
										List.iter (fun nc -> 
											if !nc = !na then (b := true; nc := !nb) else ()
										) gan;
										(*
										(if !b then Printf.printf "really subst %d : %s \n" q (virtop2str nop) else ());
										*)
										let gas = get_assigned nop in
										if List.mem !na gas || List.mem !nb gas then () else loop (q+1)
									)
								else ()
							in
							(*
							Printf.printf "%s %b %b\n" (node2str w)  head_ok.(w.idx) (v.idx = w.idx);
							*)
							if head_ok.(w.idx) then loop 0 else ();
							if v.idx = w.idx then loop (p+1) else ()
						) vs;
					)
					| _ -> ()
				) v.ops;
				
				v.dst#iter (fun w -> 
					if w.gone then ()
					else dfs1 w 
				)
			in
				dfs1 root;
			
			(* 自明なMovを取り除く *)
			
			List.iter (fun v -> 
				v.ops <- Array.of_list (Array.fold_right (fun x -> fun r -> 
					match x with
					| OpMov((na,_),(nb,_)) when !na = !nb -> r
					| _ -> x :: r
				) v.ops []) 
			) vs;
			
			(*
			Printf.printf "end_copy_anal\n";
			sl#dump_cfg ();
			*)
		)
		
		(*
		method collect_names () = (
			let res = ref [] in
			sl#idfs (fun v -> 
				res := @ !res
			);
			!res
		)
		
		
		method liveanal () = (
			(* かっせー、かいせきー。アルファ変換されてるので覚えるのは名前だけでいいはず。 *)
			sl#ungone ();
			let rec dfs1 v = 
		)
		*)
		
		method regalloc () = (
			
		)
		
		method contract () = (
			vs <- List.fold_left (fun r -> fun v -> 
				(if v.dst#size = 1 && v.dst#hd.src#size = 1 && v.dst#hd.idx <> root.idx then (
					let w = v.dst#hd in
					(* rootをマージするのは、やばいので諦めます。 *)
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
			) [] vs;
			(* 縮約したあと、idxを振りなおす *)
			let rec loop xs n = 
				match xs with
				| [] -> ()
				| x :: xs -> x.idx <- n; loop xs (n+1)
			in
				loop vs 0
		)
		
		method flatten_to_vlist () = (
			let res = ref [] in
			sl#idfs (fun v -> 
				res := (Array.to_list v.ops) @ !res
			);
			remove_useless_jump !res
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
	

let globs = ref []

let cna2na (na,td) = 
	if List.mem na !globs then (ref (GVar na),td)
		else (ref (Var na),td)

let cvs2vs vs = List.map cna2na vs

(* cfgの入り口,[出口になりうるもののリスト]、を組でかえす *)
let rec to_cfgs ast tov istail cfg fn head_label addtoroot = 
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
		to_cfgs x tov istail cfg fn head_label addtoroot
	in
	match ast with
	| CConst(x) -> sres (OpMovi(cna2na tov,x))
	| COp(x,vs) -> (
			match x,vs with
			| Syntax.Osemi1,[_] -> mres [] (* 虚無でよい *)
			| Syntax.Osemi2,[_;na] -> sres (OpMov(cna2na tov,cna2na na))
			| _ -> sres (OpOpr(cna2na tov,x,cvs2vs vs))
		)
	| CLet(na,e1,e2) -> (
			let ch1,cts1 = (to_cfgs e1 na false cfg fn head_label addtoroot) in
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
			let chd = singleton (OpJcnd(cmpty,cna2na a,cna2na b,lc2)) in
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
				sres (OpMov(cna2na tov,cna2na x))
		)
	| CApp(a,b) -> (
			sres (OpApp((if istail then Tail else NonTail),InDirApp,cna2na tov,cna2na a,cvs2vs b))
		)
	| CDirApp(a,b) -> (
			if istail && (fst a) = fn then ( 
				(* 実際に末尾再帰させる*)
				let tvs = List.map genvar cfg#args in
				let v = multiton (
					(List.map2 (fun x -> fun y -> OpMov(cna2na x,cna2na y)) tvs b) @
					(List.map2 (fun x -> fun y -> OpMov(cna2na x,cna2na y)) cfg#args tvs) @
					[OpJmp(head_label)]
				) in
				addtoroot := v :: !addtoroot; 
				v,[] 
				(* この場合、この後にretは付けなくてよい。 *)
			)
			else
				sres (OpApp((if istail then Tail else NonTail),DirApp,cna2na tov,cna2na a,cvs2vs b))
		)
	| CTuple(vs) -> (
			sres (OpMakeTuple(cna2na tov,cvs2vs vs))
		)
	| CLetTuple(vs,ta,e1) -> (
			let ch1,cts1 = (reccall e1) in
			ch1.ops <- addhead (OpDestTuple(cvs2vs vs,cna2na ta)) ch1.ops;
			(ch1,cts1)
		)
	| CClosure(na,vs) -> (
			sres (OpMakeCls(cna2na tov,na,cvs2vs vs))
		)


let cfg_toasms fn ismain args ast funnames heapvars = 
	globs := (funnames @ heapvars);
	let ncfg = new cfg_type in
	let head_label = genlabel () in
	ncfg#setargs args;
	
	let tov = if ismain then (
		("@global_ret_val",(TyInt,default_debug_data))
	) else (
		let rd = snd (snd fn) in
		let rv = ("@ret_val_" ^ (fst fn)) in
		let rt = 
			match fst (snd fn) with
			| TyFun(_,x) -> x
			| x -> raise (Failure ("Type " ^ (type2str x) ^ " is not function type"))
		in
		(rv,(rt,rd))
	)
	in
	(* opret、実体ごとに作らないといけないはず。 *)
	let retop = (fun () -> if ismain then [OpMainRet] else [OpRet(cna2na tov)]) in
	
	let addtoroot = ref [] in
	let rt,gls = to_cfgs ast tov true ncfg (fst fn) head_label addtoroot in
	rt.ops <- addhead (OpLabel(head_label)) rt.ops;
	ncfg#setroot rt;
	(* 末尾再帰をつなぐ *)
	List.iter (fun x -> 
		connect_cfg x rt
	) !addtoroot;
	ncfg#contract ();
	List.iter (fun v -> 
		 v.ops <- Array.append v.ops (Array.of_list (retop ()))
	) gls;
	(*
	*)
	if !tortesia then
		ncfg#copy_anal
		()
	else ();
	
	let res = ncfg#flatten_to_vlist () in
	(*
	print_string (String.concat "\n" (List.map virtop2str res));
	print_newline ();
	*)
	res
	

	

