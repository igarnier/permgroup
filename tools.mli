module type Comparable =
  sig

    type t

    val compare : t -> t -> int

    val equal : t -> t -> bool

    val to_string : t -> string

  end

module Int : Comparable with type t = int
module IntSet : Set.S with type elt = int
module IntMap : Map.S with type key = int

val to_sseq : ('a -> string) -> string -> 'a list -> string
val strof_ilist : int list -> string
val strof_iarr : int array -> string
val dup : 'a -> int -> 'a list
val mk_ints : int -> int -> int list
val fold_cartesian : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
