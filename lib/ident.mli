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

module StringBindings : Bindings with type t = string
module IntBindings : FreshableBindings with type t = int
module IntTypesBindings : FreshableBindings with type t = int
module StringFields : Fields with type t = string
module IntFields : FreshableFields with type t = int

