open Main_option

let main_name () = if !windows then "_main" else "_start"
let io_lib_name () = if !windows then "libio_win.s" else "libio_linux.s"
let main_heap_size () = if !windows then "0x40000000" else "0x80000000"

let main_epilogue () = if !windows then "\tret\n" else (
	"\tmov ebx,0\n" ^
	"\tmov eax,1\n" ^
	"\tint 0x80\n"
)

