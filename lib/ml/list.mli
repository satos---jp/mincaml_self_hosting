val map  : ('a -> 'b) -> 'a list -> 'b list
val iter : ('a -> unit) -> 'a list -> unit
val append : 'a list -> 'a list -> 'a list
val concat : ('a list) list -> 'a list
val mem : 'a -> 'a list -> bool
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val mem_assoc : 'a -> ('a * 'b) list -> bool
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val nth : 'a list -> int -> 'a
val length : 'a list -> int
val exists : ('a -> bool) -> 'a list -> bool
val rev : 'a list -> 'a list
val assoc : 'a -> ('a * 'b) list -> 'b
