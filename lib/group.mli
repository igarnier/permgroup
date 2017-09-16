(** [S] is the signature of a permutation group. *)
module type S =
sig

  (** [t] is the type of groups. *)
  type t

  (** [perm] is the type of permutations, i.e. elements of the group. *)
  type perm

  (** The [trivial] group. *)
  val trivial : t

  (** Functions for accessing the group. *)

  (** [mem g p] tests whether the permutation [p] belongs to [g]. *)
  val mem : t -> perm -> bool

  (** [list g] enumerates all elements of the group. Be careful, this can be quite big... *)  
  val list : t -> perm list

  (** [uniform g] samples a permutation uniformly at random. *)
  val uniform : t -> perm

  (** [order g] computes the order of the group. *)  
  val order : t -> int

  (** Functions for building permutation groups. *)

  (** [extend g p] adds [p] to [g], i.e. it computes the smallest permutation group containing
      both [p] and all elements of [g]. *)  
  val extend : t -> perm -> t

  (** [extend_mc g p] is a Monte-Carlo version of [extend g p]. It is faster, but it might
      return an incorrect result. *)  
  val extend_mc : t -> perm -> t

  (** [from_generators l] computes the smallest permutation group containing the elements of [l]. *)
  val from_generators : perm list -> t

  (** [from_generators_mc l]: see [extend_mc]. *)  
  val from_generators_mc : perm list -> t                                         
end

(** [Make] takes an implementation of [Perm.S] and returns an implementation of permutation
    groups for those permutations. *)
module Make :
  functor (Perm : Perm.S) -> (S with type perm = Perm.t)
