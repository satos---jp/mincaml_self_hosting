type debug_data = string * Lexing.position * Lexing.position

val default_debug_data : debug_data

val get_debug_data : unit -> debug_data

val debug_data2str : debug_data -> string
val debug_data2simple : debug_data -> string

val filename : string ref

val add_inscount : string -> string


