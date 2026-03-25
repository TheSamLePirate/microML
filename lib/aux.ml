
let ( << ) f g x = f (g x)
let ( >> ) g f x = f (g x)

let (--) a b = Seq.init (b - a) (fun x -> x + a) |> List.of_seq

let zip_opt xs ys =
  let rec aux acc xs ys =
    match xs, ys with
    | [], [] -> Some (List.rev acc)
    | x::xs, y::ys -> aux ((x,y)::acc) xs ys
    | _ -> None
  in
  aux [] xs ys

let group_by (li : ('a * 'b) list) : ('a * 'b list) list =
  let rec f groups = function
    | (k, v) :: tl ->
       let samek, others = List.partition (fun (k2, _v) -> k2 = k) tl in
       let values = List.map snd samek in
       f ((k, (v::values))::groups) others
    | [] -> List.rev groups
  in f [] li

let group_count cmp li =
  let rec f acc curr cnt = function
    | [] -> (curr, cnt)::acc
    | hd::tl -> if 0 = cmp hd curr then f acc curr (cnt + 1) tl
                else f ((curr, cnt)::acc) hd 1 tl
  in match List.sort cmp li with
     | hd::tl -> f [] hd 1 tl
     | [] -> []

let passacc _ acc = acc
let idacc x acc = acc, x
let didacc _d x acc = acc, x
let forall2 _ _ = true
