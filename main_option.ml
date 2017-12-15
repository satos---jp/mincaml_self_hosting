let windows = ref false
let nolib = ref false
let verbose = ref false
let tortesia = ref false
let debugmode = ref false
let noinline = ref false
let output_filename = ref "out.s"
let nooptimization = ref false
let asmsin_asmint = ref false
let all_stack = ref false

let vprint f s = 
	if !verbose then (print_string (f s); print_newline ()) else () 

let ivprint s = 
	if !verbose then (print_string s; print_newline ()) else () 

