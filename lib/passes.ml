type ('acc, 'from, 'target, 'prev, 'final ) t = {
    init : 'acc;
    process :  'acc -> 'from -> 'acc * 'target;
    final : 'acc -> 'prev -> 'final;
  }
(*
module MayDie = struct
  type ('acc, 'from, 'target, 'prev, 'final ) t_maydie = {
      init : 'acc;
      process :  'acc -> 'from -> ('acc * 'target) option;
      final : 'acc -> 'prev -> 'final;
    }
end
*)
let nofinal = fun _ a -> a

let print_pass pp = {init = (); final = nofinal; process = fun () value -> Format.printf "%a@\n" pp value; (), value;}

let no_pass = {init = (); final = nofinal; process = fun () value -> (), value}

let handle_option name access pass =
  if access name then
    let len = String.length name in
    { init = pass.init; final = pass.final;
      process = fun acc v ->
                Format.printf "%s@[" name;
                Format.pp_open_vbox Format.std_formatter 0;
                Format.pp_set_margin Format.std_formatter (Format.pp_get_margin Format.std_formatter () - len);
                let out = pass.process acc v in
                Format.pp_close_box Format.std_formatter ();
                out}
  else no_pass

let handle_exception msg = Format.eprintf "%s@\n@\n" msg

let may_fail : ('acc, 'from, 'target, 'prev, 'final ) t -> ('acc option, 'from, 'target option, 'prev, 'final option) t = fun pass ->
  {init = Some pass.init; final = (fun acc x -> Option.map (fun acc -> pass.final acc x) acc);
  process = fun acc from -> match acc with None -> None, None
                                         | Some acc ->
                                            try
                                              let acc, b = pass.process acc from in
                                              Some acc, Some b
                                            with Failure msg -> handle_exception msg; None, None}

let final_pass f = {init = (); process = (fun () itm -> (), itm); final = (fun () out -> f out; out)}

let combine : ('acc, 'from, 'mid, 'prev, 'mid2 ) t -> ('acc2, 'mid, 'target, 'mid2, 'final ) t -> ('acc1 * 'acc2, 'from, 'target, 'prev, 'final ) t = fun a b ->
  { init = a.init, b.init;
    final = (fun (acca, accb) prev ->
      let mid = a.final acca prev in
      b.final accb mid
    );
    process = fun (acca, accb) from ->
             let acca, mid = a.process acca from in
             let accb, target = b.process accb mid in
             (acca, accb), target
  }

let (|>~) list passe =
  let acc, li = List.fold_left_map passe.process passe.init list in
  passe.final acc li
let (++>) = combine
