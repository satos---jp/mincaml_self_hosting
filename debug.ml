open Lexing

type debug_data = string * Lexing.position * Lexing.position

let filename = ref ""

let default_debug_data = ("",Lexing.dummy_pos,Lexing.dummy_pos)
let get_debug_data () = (!filename, (Parsing.symbol_start_pos ()), (Parsing.symbol_end_pos ()))

let pos2str p = Printf.sprintf "%d:%d" p.pos_lnum (p.pos_cnum - p.pos_bol)

let debug_data2simple (fname,st,gl) = Printf.sprintf "%s@%s;%s" fname (pos2str st) (pos2str gl)

let debug_data2str (fname,st,gl) = Printf.sprintf "file %s %s -> %s " fname (pos2str st) (pos2str gl)

(* ラベルの後ごとに、命令数カウントを入れる *)
let add_inscount s = 
	let ts = Str.split (Str.regexp "\n") s in
	let inss c = if c = 0 then "" else (
		"\tmov edx,[inst_counter]\n" ^ 
		(Printf.sprintf "\tadd edx,%d\n" c) ^ 
		"\tmov [inst_counter],edx\n" ^
		"\tsetb dl\n" ^ 
		"\tand edx,1\n" ^
		"\tadd dword [inst_counter_up],edx\n"
	) in
	let _,res = List.fold_right (fun s -> fun (c,r) ->	
		let h = String.sub s 0 1 in
		let h2 = String.sub s 0 2 in
			if h = ";" then (c,s ^ "\n" ^ r) else 
		 	if h = "\t" then (
		 		(* jxx系だけ分ける。すなわち、 jmp,jne,jl *)
		 		if h2 = "\tj" then (1,s ^ "\n" ^ (inss c) ^ r) 
			 		else (c+1,s ^ "\n" ^ r) 
			) else (0,s ^ "\n" ^ (inss c) ^ r)
		) ts (0,"") in res




