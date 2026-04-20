open LibCore.Passes

(* tests sur les passes *)
let value = [1;2;3]
let pass_tostring = {
    init = ();
    final = (fun () a -> a);
    process = fun _ i -> (), Int.to_string i}
let pass_to_int = {
    init = ();
    final = (fun () a -> a);
    process = fun _ i -> (), int_of_string i}
let pass_add = {
    init = 0;
    final = (fun _acc a -> a);
    process = fun acc i -> acc+i, acc+i }

let () = begin
    ignore (value |>~ pass_tostring ++> pass_to_int ++> pass_add);
    ignore (value |>~
              ( may_die pass_tostring
                ++?> may_die pass_to_int
                ++?> may_die pass_add));
  end

