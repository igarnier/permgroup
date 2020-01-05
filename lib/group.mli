(** [S] is the signature of a permutation group. *)
module type S = sig
  (** [t] is the type of groups. *)
  type t

  (** [perm] is the type of permutations, i.e. elements of the group. *)
  type perm

  (** [elt] is the type of elements on which the permutation group acts. *)
  type elt

  (** [info] packs some informations about the internal representation of the permutation group.*)
  type info = {
    generators : perm list;
        (** [generators] is a list of generators for the group. *)
    stabilizers : elt list;
        (** [stabilizers] lists the stabilizers of the chain of subgroups. The length of this chain
          should be logarithmic in the size of the group. Otherwise, things will be slow. *)
    indices : int list;
        (** [indices] lists the number of cosets of each subgroup w.r.t. the stabilizers. *)
  }

  (** The function [info] returns the info for the given group. *)
  val info : t -> info

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
  val order : t -> Z.t

  (** Functions for building permutation groups. *)

  (** [extend g p] adds [p] to [g], i.e. it computes a representation of the smallest permutation group containing
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
module Make (Perm : Perm.S) : S with type perm = Perm.t and type elt = Perm.elt
