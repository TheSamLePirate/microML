type 'a list = Cons of 'a * 'a list | Nil


let _ =
  let id x = x in
  let dupp x = (x, x) in
  let a = id 4 in
  let b = id Nil in
  let c = dupp 0 in
  let d = dupp Nil in
  0
