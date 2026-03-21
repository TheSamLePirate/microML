type 'a printer = Format.formatter -> 'a -> unit
val sep :(unit, Format.formatter, unit) format -> unit printer
val int : int printer
val string : string printer
val prefix : string -> 'a printer -> 'a printer
val listi : unit printer -> (Format.formatter -> int -> 'a -> unit) -> 'a list printer
val list : unit printer -> 'a printer -> 'a list printer
val failwith : ('a, Format.formatter, unit, 'b) format4 -> 'a
val assoc : 'a printer -> 'b printer -> ('a * 'b) list printer
