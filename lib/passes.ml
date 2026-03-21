type ('acc, 'from, 'target ) t = {
    init : 'acc;
    process :  'acc -> 'from -> 'acc * 'target
  }

let print_pass pp =
  {init = ();
   process = fun () value -> Format.printf "%a@\n" pp value; (), value }

let no_pass = {init = (); process = fun () value -> (), value }

let handle_option name access pass () =
  if access name then
    begin Format.printf "Passe : %s@\n" name; pass end
  else no_pass

(* TODO gérer un genre d'option, gestion des erreurs, avancer petit à petit*)
let (>*>) ast passe = List.fold_left_map passe.process passe.init ast
let (>|>) ast passe = snd (ast >*> passe)
let (>|?>) ast passe = ast >|> passe ()
