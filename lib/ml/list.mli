val map  : ('a -> 'b) -> 'a list -> 'b list
val iter : ('a -> unit) -> 'a list -> unit
val append : 'a list -> 'a list -> 'a list
val concat : ('a list) list -> 'a list
val mem : 'a -> 'a list -> bool
