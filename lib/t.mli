type ('a, 'acc, 'b) fun_foldmap = 'a -> 'acc -> 'acc * 'b
type ('a, 'acc) fold = 'a -> 'acc -> 'acc
type 'a equals = 'a -> 'a -> bool
type 'a compare = 'a -> 'a -> int
