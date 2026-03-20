module type Bindings = sig
  include Map.OrderedType
  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end
module type Fields = sig
  include Map.OrderedType
  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end
module type FreshableFields = sig
  include Fields
  val init : t
  val next : t -> t
end
module type FreshableBindings = sig
  include Bindings
  val init : t
  val next : t -> t
end

module StringBindings : Bindings with type t = string = struct
  include String
  module Map = Map.Make(String)
  module Set = Set.Make(String)
  let pp = Lib.Pp.string
end
module IntBindings : FreshableBindings with type t = int = struct
  include Int
  module Map = Map.Make(Int)
  module Set = Set.Make(Int)
  let pp f i = Format.fprintf f "v%d" i
  let init = 0
  let next n = n + 1
end
module IntTypesBindings : FreshableBindings with type t = int = struct
  include Int
  module Map = Map.Make(Int)
  module Set = Set.Make(Int)
  let pp = Ast.Ty.pp_tyvar
  let init = 0
  let next n = n + 1
end
module StringFields : Fields with type t = string = struct
  include String
  module Map = Map.Make(String)
  module Set = Set.Make(String)
  let pp = Lib.Pp.string
end
module IntFields : FreshableFields with type t = int = struct
  include Int
  module Map = Map.Make(Int)
  module Set = Set.Make(Int)
  let pp f i = Format.fprintf f "f%d" i
  let init = 0
  let next n = n + 1
end
