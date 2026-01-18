let out =
  let optmap f x = match x with
    | None -> None
    | Some x -> Some (f x)
  in optmap (fun x -> x + 1) (Some 5)
