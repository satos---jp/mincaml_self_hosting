open Main_option

let main_name () = if !windows then "_main" else "_start"
let io_lib_name () = if !windows then "libio_win.s" else "libio_linux.s"
let main_heap_size () = if !windows then "0x40000000" else "0x80000000"

let main_epilogue () = if !windows then "\tret\n" else (
	let eprintc x = (
		(Printf.sprintf "\tpush %d\n" x) ^
		"\tcall print_char_err\n" ^
		"\tadd esp,4\n"
	) in
	(if !debugmode then (
		(eprintc 105) ^
		(eprintc 99) ^
		(eprintc 32) ^
		"\tmov eax,[inst_counter_up]\n" ^ 
		"\tpush eax\n" ^ 
		"\tcall print_hex_err\n" ^
		"\tadd esp,4\n" ^
		"\tmov eax,[inst_counter]\n" ^ 
		"\tpush eax\n" ^ 
		"\tcall print_hex_err\n" ^
		"\tadd esp,4\n" ^
		(eprintc 10)
	) else "") ^
	"\tmov ebx,0\n" ^
	"\tmov eax,1\n" ^
	"\tint 0x80\n"
)

