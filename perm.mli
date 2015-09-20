module type S =
  sig
    type t
    val equal : t -> t -> bool
    val identity : t
    val prod : t -> t -> t
    val inv : t -> t
    val action : t -> int -> int
    val orbit : t -> int -> Tools.IntSet.t
    val pick_from_support : t -> int option
    val of_cycles : int array list -> t
    val of_array : int array -> t
    val print : t -> string
  end

module CycleBased : S

module ArrayBased : functor (Size : sig val size : int end) -> S

module type PermType =
  sig
    module Concrete : S
    type permrec = { p : Concrete.t; invp : Concrete.t; }
    type t = Perm of permrec | Prod of t * t | Inv of t
    val of_concrete : Concrete.t -> t
    val normalise : t -> t
    val identity : t
    val is_identity : t -> bool
    val invert : t -> t
    val power : t -> int -> t
    val action : t -> int -> int
    val invert_action : t -> int -> int
    val orbit : t list -> int list -> t Tools.IntMap.t
    val of_cycles : int array list -> t
    val print : t -> string
    val print_orbit : t Tools.IntMap.t -> string

    module Operators :
      sig val ( *** ) : t -> t -> t val ( ^^ ) : int -> t -> int end
  end

module Make : functor (Concrete : S) -> PermType
