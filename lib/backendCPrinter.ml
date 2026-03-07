open AstBackend

module Make(B:Ast.Bindings) = struct

  let pp_atom f = function
    | Fixable.Native n -> Format.fprintf f "%s" n
    | Fixable.GlobalVar v | Var v -> B.pp f v
    | Fixable.Int i -> Format.fprintf f "make_int(%d)" i
    | Fixable.Char c -> Format.fprintf f "make_char(%c)" c
    | Fixable.Bool b -> Format.fprintf f "make_bool(%b)" b
    | Fixable.Unit -> Format.fprintf f "make_unit()"

  let pp_rhs f = function
    | Fixable.Atom a -> pp_atom f a
    | Fixable.Prim1 (op, atm) ->
       Format.fprintf f
         (match op with
          | Neg -> "int_neg(%a)"
          | Not -> "bool_not(%a)")
         pp_atom atm
    | Fixable.Prim2 (op, atm1, atm2) ->
       Format.fprintf f
         (match op with
          | Add -> "int_add(%a, %a)"
          | Sub -> "int_sub(%a, %a)"
          | Mul -> "int_mul(%a, %a)"
          | Div -> "int_div(%a, %a)"
          | Eq -> "mlv_eq(%a, %a)"
          | Neq -> "mlv_neq(%a, %a)"
          | Lt -> "int_lt(%a, %a)"
          | Le -> "int_le(%a, %a)"
          | Gt -> "int_gt(%a, %a)"
          | Ge -> "int_ge(%a, %a)"
          | And -> "bool_and(%a, %a)" (* pas besoin si on fait la passe pour transformer en if *)
          | Or -> "bool_or(%a, %a)"
         )
         pp_atom atm1
         pp_atom atm2
    | Fixable.Construct (d, atom) -> Format.fprintf f "construct(%d, %a)" d pp_atom atom
    | Fixable.Destruct (d, atom) -> Format.fprintf f "destruct(%d, %a)" d pp_atom atom
    | Fixable.MakeTuple [] -> Format.fprintf f "make_tuple(0)"
    | Fixable.MakeTuple(atoms) -> Format.fprintf f "make_tuple(%d, %a)" (List.length atoms) (Lib.Pp.list (Lib.Pp.sep ",@ ") pp_atom) atoms
    | Fixable.Nth (i, atom) -> Format.fprintf f "tuple_nth(%d, %a)" i pp_atom atom
    | Fixable.MakeRecord(li) -> Format.fprintf f "make_record(%d, %a)" (List.length li) (Lib.Pp.list (Lib.Pp.sep ",@ ") (fun f (field, atom) -> Format.fprintf f "%d, %a" field pp_atom atom)) li
    | Fixable.Proj(int, atom) -> Format.fprintf f "record_get(%a, %d)" pp_atom atom int
    | Fixable.CallDirect(funid, atoms) -> Format.fprintf f "%a(%a)" B.pp funid (Lib.Pp.list (Lib.Pp.sep ",@ ") pp_atom) atoms
    | Fixable.CallClosure (atom, atoms) -> Format.fprintf f "clot_call(%a, %d, %a)" pp_atom atom (List.length atoms) (Lib.Pp.list (Lib.Pp.sep ",@ ") pp_atom) atoms
    | Fixable.MakeClosure (funid, arity, []) -> Format.fprintf f "clot_make(%a_clot, %d, 0)" B.pp funid arity
    | Fixable.MakeClosure (funid, arity, atoms) -> Format.fprintf f "clot_make(%a, %d, %d, %a)" B.pp funid arity (List.length atoms) (Lib.Pp.list (Lib.Pp.sep ",@ ") pp_atom) atoms


  let pp_t e f letifopt = match e with
    | Fixable.Let (var, rhs, e) -> Format.fprintf f "MLV %a = %a;@\n%a" B.pp var pp_rhs rhs e letifopt
    | Fixable.LetIf (var, atom, ethen, eelse, next) ->
       Format.fprintf f "MLV %a@\nif (%a){@\n@[<v2>  %a@]@\n}else{@\n@[<v2>  %a@]}@\n%a"
         B.pp var
         pp_atom atom
         ethen (Some var)
         eelse (Some var)
         next letifopt
    | Fixable.LetIfTag (var, atom, i, ethen, eelse, next) ->
       Format.fprintf f "MLV %a;@\nif (is_constructed(%d, %a)){@\n@[<v2>  %a@]@\n}else{@\n@[<v2>  %a@]@\n}@\n%a"
         B.pp var i
         pp_atom atom
         ethen (Some var)
         eelse (Some var)
         next letifopt
    | Fixable.AssertFalse -> Format.fprintf f "assert(0);"
    | Fixable.Return atom ->
       match letifopt with
       | None -> Format.fprintf f "return %a;" pp_atom atom
       | Some s -> Format.fprintf f "%a = %a;" B.pp s pp_atom atom

  let pp_t0 e f opt = Fixed.Deep.reduce pp_t e f opt
  let pp_t f e = pp_t0 e f None

  let pp_function f func = Format.fprintf f "MLV %a(%a){@\n@[<v 2>  %a@]@\n}@\nMLV %a_clot(MLV args){@\n  return %a(%a);@\n}@\n" B.pp func.id
                                 (Lib.Pp.list (Lib.Pp.sep ", ")
                                    (fun f v -> Format.fprintf f "MLV %a" B.pp v)) func.args
                         pp_t func.body
                         B.pp func.id
                         B.pp func.id
                         (Lib.Pp.listi (Lib.Pp.sep ", ")
                            (fun f i v -> Format.fprintf f "args.ptr[%d] /*%a*/" i B.pp v)) func.args
  let pp_entry_decl f (var, _itm) = Format.fprintf f "MLV %a;" B.pp var
  let pp_entry_init f (var, itm) = pp_t0 itm f (Some var)

  let process env program =
    let env = (fun f () -> Lib.Pp.list (Lib.Pp.sep "@\n") pp_entry_init f program.entry) :: env in
    env, fun f () ->
    Format.fprintf f "%a@\n%a@\n"
      (Lib.Pp.list (Lib.Pp.sep "@\n") pp_function) program.functions
      (Lib.Pp.list (Lib.Pp.sep "@\n") pp_entry_decl) program.entry

  let passe = {
      Ast.init = [ ];
      process = process
  }
end
