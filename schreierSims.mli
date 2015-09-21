module type S =
  sig
    type t
    type perm

    val mem : t -> perm -> bool
    val list : t -> perm list
    val uniform : t -> perm
    val order : t -> int
    val extend : t -> perm -> t
    val from_generators : perm list -> t
  end

module Make : functor (Perm : Perm.S) -> S with type perm = Perm.t
