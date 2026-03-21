type 'a printer = Format.formatter -> 'a -> unit
let sep format f () = Format.fprintf f format
let int f i = Format.fprintf f "%d" i
let string f s = Format.fprintf f "%S" s
let prefix s p f x = Format.fprintf f "%s%a" s p x
let listi sep pp f li =
  let rec print i f = function
    | [] -> ()
    | [hd] -> pp f i hd
    | hd::((_::_2) as tl) ->
       Format.fprintf f "%a%a%a" (fun f hd -> pp f i hd) hd sep () (print (i+1)) tl
  in print 0 f li
let list sep pp f li = listi sep (fun f _i v -> pp f v) f li
let failwith fmt = Format.kasprintf failwith fmt
let assoc ppa ppb f li =
  Format.fprintf f "{ @[<v>%a@]}"
    (list (fun f () -> Format.fprintf f ";@\n")
       (fun f (a, b) -> Format.fprintf f "%a=%a" ppa a ppb b)) li
