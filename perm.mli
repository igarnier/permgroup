module type S =
  sig

    (* The type of elements on which the permutations act *)
    module E : Tools.Comparable
    module Set : Set.S with type elt = E.t

    type elt = E.t
           
    type t

    (* Equality test *)
    val equal : t -> t -> bool

    (* identity takes as an argument the size of the set on which PermSig acts. *)
    val identity : t

    (* product *)
    val prod     : t -> t -> t

    (* inverse *)
    val inv      : t -> t

    (* action *)
    val action   : t -> elt -> elt

    (* orbit *)
    val orbit    : t -> elt -> Set.t

    (* any point not fixed by the perm. *)
    val pick_from_support : t -> elt option

    val of_cycles : elt array list -> t

    val of_mapping : (elt * elt) list -> t

    val print : t -> string

  end

module CycleBased : functor (Elt : Tools.Comparable) -> S with type E.t = Elt.t

(* Hashed variant *)
(* module HCycleBased : functor (Elt : Tools.ComparableAndHashable) -> S with type E.t = Elt.t                                                                             *)

module ArrayBased : functor (Size : sig val size : int end) -> S with type E.t = int

(* module type PermType = *)
(*   sig *)
(*     module Concrete : S *)
(*     type permrec = { p : Concrete.t; invp : Concrete.t; } *)
(*     type t = Perm of permrec | Prod of t * t | Inv of t *)
(*     val of_concrete : Concrete.t -> t *)
(*     val normalise : t -> t *)
(*     val identity : t *)
(*     val is_identity : t -> bool *)
(*     val invert : t -> t *)
(*     val power : t -> int -> t *)
(*     val action : t -> int -> int *)
(*     val invert_action : t -> int -> int *)
(*     val orbit : t list -> int list -> t Tools.IntMap.t *)
(*     val of_cycles : int array list -> t *)
(*     val print : t -> string *)
(*     val print_orbit : t Tools.IntMap.t -> string *)

(*     module Operators : *)
(*       sig val ( *** ) : t -> t -> t val ( ^^ ) : int -> t -> int end *)
(*   end *)

(* module Make : functor (Concrete : S) -> PermType *)
