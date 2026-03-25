val ( << ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( -- ) : int -> int -> int list
val zip_opt : 'a list -> 'b list -> ('a * 'b) list option
val group_by : ('a * 'b) list -> ('a * 'b list) list
val group_count : ('a -> 'a -> int) -> 'a list -> ('a * int) list
val passacc : 'a -> 'b -> 'b
val idacc : 'a -> 'b -> 'b * 'a
val didacc : 'a -> 'b -> 'c -> 'c * 'b
