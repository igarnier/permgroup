module type S =
  sig
    type t
    type perm
    type elt

    (* Accessing the group *)           
    val mem : t -> perm -> bool
    val list : t -> perm list
    val uniform : t -> perm
    val order : t -> int

    (* Building the group *)
    val extend : t -> perm -> t
    val extend_mc : t -> perm -> t
    val from_generators : perm list -> t
    val from_generators_mc : perm list -> t                                         
  end
    
module Make :
  functor (Perm : Perm.S) -> (S with type perm = Perm.t and type elt = Perm.E.t)
