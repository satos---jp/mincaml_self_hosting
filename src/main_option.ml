let windows = ref false
let nolib = ref false
let verbose = ref false
let debugmode = ref false
let noinline = ref false
let output_filename = ref "a.out"
let nooptimization = ref false
let asmsin_asmint = ref false
let check_array_boundary = ref false

(* tortesia用のオプションだった *)
let tortesia = ref false
let all_stack = ref false
let in_out_assembler = ref false


let output_assembler = ref false

let argparse files = 
	Arg.parse [
		("-nolib",Arg.Set nolib,"stop including lib.ml");
		("-v",Arg.Set verbose,"verbose debug info");
		("-w",Arg.Set windows,"compile for windows x86");
		("-d",Arg.Set debugmode,"debug inscount on");
		("-noopt",Arg.Set nooptimization,"stop optimization");
		("-noinline",Arg.Set noinline,"stop inlining");
		("-o",Arg.Set_string output_filename,"output filename");
		("-asi",Arg.Set asmsin_asmint,"use x86 tirgonal and x86 print_int");
		("-cab",Arg.Set check_array_boundary,"check boundary of array for x86");
		
		("-s",Arg.Set output_assembler,"output .s file");
				
	] (fun fn -> files := (!files) @ [fn]) (Printf.sprintf "Usage: %s filename\n" Sys.argv.(0))

let vprint f s = 
	if !verbose then (print_string (f s); print_newline ()) else () 

let ivprint s = 
	if !verbose then (print_string s; print_newline ()) else () 

let fvprint f = 
	if !verbose then (f (); print_newline ()) else () 

