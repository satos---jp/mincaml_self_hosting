let windows = ref false

let main_name () = if !windows then "_main" else "main"
let io_lib_name () = if !windows then "libio_win.s" else "libio_linux.s"



