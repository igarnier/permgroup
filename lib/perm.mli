(** This module implements operations on permutations. Various applications
  require different implementation of permutations, so we propose various
  "concrete" implementations of the "S" signature (disjoint cycle-based or
  array-based). *)

open Permtools

module type Element_sig = sig
  include Ordered_sig

  include Hashable_sig with type t := t

  include Pp_sig with type t := t
end

(** [S] is the module type of permutations. We propose two implementations of [S]:
    [CycleBased] and [ArrayBased]. *)
module type S = sig
  (* The type of elements on which the permutations act *)
  module E : Element_sig

  module Set : Set.S with type elt = E.t

  type elt = E.t

  type t

  (* Equality test *)
  val equal : t -> t -> bool

  (* identity takes as an argument the size of the set on which PermSig acts. *)
  val identity : t

  (* product *)
  val prod : t -> t -> t

  (* inverse *)
  val inv : t -> t

  (* action *)
  val action : t -> elt -> elt

  (* orbit *)
  val orbit : t -> elt -> Set.t

  (* any point not fixed by the perm. *)
  val pick_from_support : t -> elt option

  val of_cycles : elt array list -> t

  val of_mapping : (elt * elt) list -> t

  val to_mapping : t -> (elt * elt) list

  include Pp_sig with type t := t

  include Hashable_sig with type t := t
end

(** [CycleBased] is a functor associating any [Permtools.Ordered_sig] element type to permutations
    acting on the set of those elements. Since [CycleBased] permutations are implemented as
    a cycle decomposition with cycles implemented as arrays, these are more memory-efficient
    than the [ArrayBased] ones: the points wich are fixed by the permutations are not stored.
    Moreover, the persistency allows for sharing. Computing the action of a permutation on a
    point is logarithmic in the size of the support of the permutation. Inversion and
    products are more costly than in the [ArrayBased] implementation. *)
module Cycle_based (Elt : Element_sig) : S with type E.t = Elt.t

(** [ArrayBased] is a functor that takes a [Size] as an argument and that builds a module
    of permutations on the set of integers between 0 and [Size.size-1]. These permutations
    are implemented as [int arrays]. They are potentially less memory efficient that the
    [CycleBased] ones, especially for permutations which have a small support. Acting
    on elements with those permutations is very fast, since it corresponds to accessing
    an array element. *)
module Array_based (Size : sig
  val size : int
end) : S with type E.t = int

module Hash_consed (X : S) : S with type E.t = X.E.t
