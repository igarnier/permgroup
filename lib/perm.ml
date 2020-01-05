(*
 * Permutation implementations based on cycles or arrays.
 * Copyright (C) 2015 Ilias Garnier
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Permtools

(* The abstract signature of a permutation implementation *)
module type S = sig
  (* The type of elements on which the permutations act *)
  module E : Permtools.Comparable

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

  (* val of_array  : int array -> t *)

  val to_mapping : t -> (elt * elt) list

  val print : t -> string
end

(* --------------------------------------------------------
   Concrete, persistent perms implemented by disjoint cycles.
   They are more memory efficient than the array-based ones:
   the points wich are fixed by the permutations are not stored.
   Moreover, the persistency allows for sharing.
   Computing the action of a permutation on a point is logarithmic
   in the size of the support of the permutation. Inversion and
   products are quite costly though.

   The module DisjointCycles should implement PermSig.
 *)

module CycleBased (Elt : Permtools.Comparable) : S with type E.t = Elt.t =
struct
  module E = Elt
  module Set = Set.Make (E)
  module Map = Map.Make (E)

  type elt = Elt.t

  (* Invariants:
     1. cycles are nonempty arrays
     2. 1-element cycles are omitted *)
  type cycle = elt array

  (* A perm is a list of /disjoint/ cycles -- i.e. a set of cycles,
   * implemented as a map from elements to pairs of
   * (cycles containing those elements, image through the permutation).
   * Sharing allows to not waste too much memory, and we compute the
   * orbit of an element in O(log(n)) (the access time) *)
  type t = (cycle * elt) Map.t

  (* Identity permutation *)

  let cycles_equal cyc1 cyc2 =
    let len1 = Array.length cyc1 in
    let len2 = Array.length cyc2 in
    len1 = len2
    &&
    let i = ref 0 in
    let res = ref true in
    while !i < len1 do
      if not (E.equal cyc1.(!i) cyc2.(!i)) then (
        res := false ;
        i := len1 )
      else () ;
      incr i
    done ;
    !res

  let equal p1 p2 =
    Map.equal
      (fun (cyc1, im1) (cyc2, im2) ->
        E.equal im1 im2 && cycles_equal cyc1 cyc2)
      p1
      p2

  let identity = Map.empty

  let rec _image_cycle_aux first point = function
    | [] ->
        failwith "Perm.image_cycle_aux: bug found"
    | [x] ->
        if E.equal x point then first
        else failwith "Perm.image_cycle_aux: bug found"
    | x :: (y :: _tl as ytl) ->
        if E.equal x point then y else _image_cycle_aux first point ytl

  let image_cycle point cyc =
    let res = ref point in
    let len = Array.length cyc in
    for i = 0 to len - 1 do
      if E.equal cyc.(i) point then res := cyc.((i + 1) mod len)
    done ;
    !res

  (* Image of an element through a perm *)
  (* O(log(n)) *)
  let image x perm = try snd (Map.find x perm) with Not_found -> x

  (* Orbit of an element through a perm, i.e. cycle of the elt *)
  let orbit (perm : t) (x : elt) =
    try
      let a = fst (Map.find x perm) in
      Array.fold_right Set.add a Set.empty
    with Not_found -> Set.singleton x

  let pick_from_support (perm : t) =
    try
      let (x, _) = Map.choose perm in
      Some x
    with Not_found -> None

  (* Product of two permutations and related functions *)
  let rec compute_cycle_aux p1 p2 first i acc =
    let i' = image i p1 in
    let j = image i' p2 in
    if j = first then List.rev acc
    else compute_cycle_aux p1 p2 first j (j :: acc)

  let compute_cycle p1 p2 i = Array.of_list (compute_cycle_aux p1 p2 i i [i])

  (* Product of two permutations. This is relatively costly. *)
  let prod_aux p1 p2 i acc =
    if not (Map.mem i acc) then
      let cyc = compute_cycle p1 p2 i in
      let len = Array.length cyc in
      if len = 0 then failwith "Perm.prod: empty cycle"
      else if len > 1 then
        (* omit unit sized cycles *)
        Array.fold_left
          (fun map x -> Map.add x (cyc, image_cycle x cyc) map)
          acc
          cyc
      else acc
    else acc

  let prod p1 p2 =
    (* We only have to fold over the union of the domains of p1 and p2 *)
    let acc1 =
      Map.fold (fun key _ acc -> prod_aux p1 p2 key acc) p1 Map.empty
    in
    Map.fold (fun key _ acc -> prod_aux p1 p2 key acc) p2 acc1

  (* Inverse of a cycle. *)
  let inv_cycle cyc =
    let arr = Array.copy cyc in
    let len = Array.length arr in
    for i = 0 to len / 2 do
      let tmp = arr.(i) in
      arr.(i) <- arr.(len - i - 1) ;
      arr.(len - i - 1) <- tmp
    done ;
    arr

  (* A wrapper to ensure no cycle is duplicated. Reduces memory waste.
   * A hashtable is used to intercept duplicates. The size of the hashtable is
   * set to [size]: in average, a permutation has log(size) different cycles so
   * collisions should be pretty rare (for big enough size).
   * This is set with the assumption that size won't be enormous either. *)
  let inv_cycle_persistent size =
    let table = Hashtbl.create size in
    fun cyc ->
      try Hashtbl.find table cyc
      with Not_found ->
        let icyc = inv_cycle cyc in
        Hashtbl.add table cyc icyc ; icyc

  (* Inverse of a permutation. *)
  let inv (p : t) =
    let invert_cycle = inv_cycle_persistent (Map.cardinal p) in
    Map.fold
      (fun key (cycle, point) acc ->
        Map.add point (invert_cycle cycle, key) acc)
      p
      Map.empty

  (* compute the image of a point through a perm, i.e. the natural action
   * of the perm. *)
  let action perm point = image point perm

  let _max_of_array (arr : int array) =
    let acc = ref min_int in
    for i = 0 to Array.length arr - 1 do
      acc := max !acc arr.(i)
    done ;
    !acc

  let add_mapping perm point image cycle = Map.add point (cycle, image) perm

  let rec push_cyc_aux cyc len i acc =
    if i = len then acc
    else
      let acc = add_mapping acc cyc.(i) cyc.((i + 1) mod len) cyc in
      push_cyc_aux cyc len (i + 1) acc

  let push_cyc perm cyc =
    let len = Array.length cyc in
    push_cyc_aux cyc len 0 perm

  (* Create a perm from a list of /disjoint/ cycles. Notice we don't check for consistency. *)
  let of_cycles cycles = List.fold_left push_cyc identity cycles

  (* Create a perm from an array-based repr *)
  let rec cycle_of m elt acc =
    if List.mem elt acc then List.rev acc
    else
      let im = List.assoc elt m in
      cycle_of m im (elt :: acc)

  let rec of_mapping_aux m perm =
    match m with
    | [] ->
        perm
    | (x, y) :: tail ->
        if Map.mem x perm || E.equal x y then of_mapping_aux tail perm
        else
          let cyc = Array.of_list (cycle_of m x []) in
          push_cyc perm cyc

  let of_mapping m = of_mapping_aux m identity

  let to_mapping perm =
    Map.fold (fun key (_, elt) acc -> (key, elt) :: acc) perm []

  (* let rec of_array_aux arr i acc = *)
  (*   if i >= Array.length arr then acc *)
  (*   else if Map.mem i acc then *)
  (*     of_array_aux arr (i+1) acc *)
  (*   else *)
  (*     let cyc = Array.of_list (cycle_of_elt arr i []) in *)
  (*     let acc = push_cyc acc cyc in *)
  (*     of_array_aux arr (i+1) acc *)

  (* let of_array arr = *)
  (*   let _ = failwith "TODO: does not preserve canonicity of identity perm" in *)
  (*   of_array_aux arr 0 identity *)

  let _max_elt x y =
    let c = E.compare x y in
    if c < 0 then y else x

  let print perm =
    if Map.is_empty perm then "id"
    else
      let bindings =
        Map.fold (fun key (_, image) acc -> (key, image) :: acc) perm []
      in
      let (dom, codom) = List.split bindings in
      let doms = Permtools.to_sseq E.to_string " " dom in
      let codoms = Permtools.to_sseq E.to_string " " codom in
      Printf.sprintf "%s\n%s\n" doms codoms
end

(* (\* Hashed variant *\)     *)
(* module HCycleBased(Elt : Permtools.ComparableAndHashable) : S with type E.t = Elt.t = *)
(*   struct *)

(*     module C = CycleBased(Elt) *)

(*     module E = C.E *)

(*     module Set = C.Set *)

(*     type t = *)
(*       { *)
(*         hash : int; *)
(*         perm : C.t *)
(*       } *)

(*     let hash_cycle cyc = *)
(*       Array.fold_left (fun acc elt -> *)

(*                       ) *)

(*     let hash (perm : C.t) = *)
(*       C.Map.fold (fun key (cycle, elt) -> *)

(*     let equal perm1 perm2 = *)
(*       perm1.hash = perm2.hash && *)
(*       C.equal perm1.perm perm2.perm *)

(*     let identity = *)
(*       { *)
(*         hash = hash C.identity; *)
(*         perm = C.identity *)
(*       } *)

(*     let prod perm1 perm2 = *)
(*       { *)

(*       } *)

(*   end *)

module ArrayBased (Size : sig
  val size : int
end) =
struct
  let size = Size.size

  module E = Permtools.Int
  module Set = Permtools.IntSet

  type elt = int

  type t = int array

  let equal p1 p2 = p1 = p2

  let identity = Array.init size (fun x -> x)

  let prod a b = Array.init (Array.length a) (fun x -> b.(a.(x)))

  let inv a =
    let size = Array.length a in
    let x = Array.make size 0 in
    for i = 0 to size - 1 do
      x.(a.(i)) <- i
    done ;
    x

  let action x i = x.(i)

  (* let rec push_cyc fst cyc arr =
   *   match cyc with
   *   | [] -> ()
   *   | [x] ->
   *      arr.(x) <- fst
   *   | x :: ((y :: _tl) as tail) ->
   *      arr.(x) <- y;
   *      push_cyc fst tail arr *)

  let push_cyc cyc arr =
    let len = Array.length cyc in
    for i = 0 to len - 2 do
      arr.(cyc.(i)) <- cyc.(i + 1)
    done ;
    arr.(cyc.(len - 1)) <- cyc.(0)

  let rec of_cycles_aux cycles arr =
    match cycles with
    | [] ->
        arr
    | cyc :: tail ->
        let len = Array.length cyc in
        if len = 0 then failwith "empty cycle"
        else if len = 1 then of_cycles_aux tail arr
        else (push_cyc cyc arr ; of_cycles_aux tail arr)

  let of_cycles cycles =
    let size =
      List.fold_left
        (fun mx cyc -> max mx (Array.fold_left max 0 cyc))
        0
        cycles
    in
    let a = Array.init size (fun x -> x) in
    of_cycles_aux cycles a

  (* let of_array x = x *)

  let rec orbit_aux perm x acc =
    if IntSet.mem x acc then acc
    else orbit_aux perm perm.(x) (IntSet.add x acc)

  let orbit (perm : t) (x : int) = orbit_aux perm x IntSet.empty

  let rec pick_from_support_aux (perm : t) i =
    if i = Array.length perm then None
    else if perm.(i) <> i then Some i
    else pick_from_support_aux perm (i + 1)

  let pick_from_support perm = pick_from_support_aux perm 0

  let of_mapping m =
    let len = List.length m in
    if len <> size then failwith "Perm.of_mapping: wrong size"
    else
      let p = Array.copy identity in
      for i = 0 to len - 1 do
        p.(i) <- List.assoc i m
      done ;
      p

  let to_mapping =
    let rec loop arr i acc =
      if i = Array.length arr then acc
      else loop arr (i + 1) ((i, arr.(i)) :: acc)
    in
    fun arr -> loop arr 0 []

  let print x = Permtools.strof_iarr x
end

(* ------------------------------------------------ *)
(* Lifting concrete operations to permutation words *)

(* module type PermType =
 *   sig
 *     module Concrete : S
 *     type permrec = { p : Concrete.t; invp : Concrete.t }
 *     type t = Perm of permrec | Prod of t * t | Inv of t
 *
 *     val of_concrete : Concrete.t -> t
 *     val normalise : t -> t
 *     val identity : t
 *     val is_identity : t -> bool
 *     val invert : t -> t
 *     val power : t -> int -> t
 *     val action : t -> int -> int
 *     val invert_action : t -> int -> int
 *     val orbit : t list -> int list -> t IntMap.t
 *     val of_cycles : int array list -> t
 *     val print : t -> string
 *     val print_orbit : t IntMap.t -> string
 *
 *     module Operators :
 *     sig
 *       val ( *** ) : t -> t -> t
 *       val ( ^^ ) : int -> t -> int
 *     end
 *
 *   end *)

(* module Make(Concrete : S) = *)
(*   (struct *)

(*       module Concrete = Concrete *)

(*       type permrec =  *)
(*         { p       : Concrete.t; *)
(*           invp    : Concrete.t; } *)

(*       (\* We want to avoid computing products and inverses unless we really need to. *)
(*        * Products of perms are simply trees of perms, and we compute the product only *)
(*        * when explicitly required. We tag each node with the support of the perm. *\) *)
(*       type t =  *)
(*         | Perm of permrec *)
(*         | Prod of t * t *)
(*         | Inv  of t *)

(*       (\* Normalise a perm *\) *)
(*       let rec normalise_aux x = *)
(*         match x with *)
(*         | Perm p -> p *)
(*         | Prod(l, r) -> *)
(*            let nl = normalise_aux l in *)
(*            let nr = normalise_aux r in *)
(*            { p    = Concrete.prod nl.p nr.p; *)
(*              invp = Concrete.prod nr.invp nl.invp } *)
(*         | Inv p -> *)
(*            let np = normalise_aux p in *)
(*            { p = np.invp; invp = np.p } *)

(*       let of_concrete (p : Concrete.t) = *)
(*         Perm { p    = p; *)
(*                invp = Concrete.inv p } *)

(*       let normalise x = Perm (normalise_aux x) *)

(*       let identity = Perm { p = Concrete.identity; invp = Concrete.identity } *)

(*       let is_identity x = *)
(*         (normalise_aux x).p = Concrete.identity *)

(*       (\* Moderately smart constructor (still O(1)) *\) *)
(*       let invert p = *)
(*         match p with *)
(*         | Inv p' -> p' *)
(*         | _ -> Inv p *)

(*       let rec power x n = *)
(*         if n = 0 then identity *)
(*         else if n = 1 then x *)
(*         else if n mod 2 = 0 then *)
(*           let px = power x (n/2) in *)
(*           Prod(px, px) *)
(*         else *)
(*           let px = power x (n/2) in *)
(*           Prod(Prod(px, px),px) *)

(*       let rec action perm point = *)
(*         match perm with *)
(*         | Perm p -> Concrete.action p.p point *)
(*         | Prod(l, r) -> *)
(*            action r (action l point) *)
(*         | Inv p -> *)
(*            invert_action p point *)

(*       and invert_action perm point = *)
(*         match perm with *)
(*         | Perm p -> Concrete.action p.invp point *)
(*         | Prod(l, r) -> *)
(*            invert_action l (invert_action r point) *)
(*         | Inv p -> *)
(*            action p point *)

(*       (\* Compute the orbit of a set of elements, and for each point in the *)
(*      orbit, a transversal. Another slower but more compact method would be to *)
(*      use a Schreier tree (i.e. a prefix tree with paths labelled by permutation  *)
(*      words). Storing the full transversal allows for direct access to *)
(*      its elements.  *)

(*      TODO: possibly more efficient algo, taking advantage of the cycles stored *)
(*      in the perm. Each elt of the cycle corresponds to a particular power of *)
(*      the perm acting on the considered point. This gives for each point and each *)
(*      group element the complete set of all possible transitions to other points. *)
(*      Orbit is then the connected component of a point. *)

(*      NOTE: in the following algo, we forget the orignating point and return *)
(*      only for each point in the orbit the corresponding transversal. *)
(*        *\) *)

(*       let rec orbit_aux group queue transversal = *)
(*         if Queue.is_empty queue then *)
(*           transversal *)
(*         else *)
(*           let (point, u) = Queue.take queue in *)
(*           if IntMap.mem point transversal then *)
(*             orbit_aux group queue transversal *)
(*           else *)
(*             (let transversal = IntMap.add point u transversal in *)
(*              List.iter (fun g -> Queue.add (action g point, Prod(u, g)) queue) group; *)
(*              orbit_aux group queue transversal) *)

(*       let orbit group points = *)
(*         let queue = Queue.create () in *)
(*         List.iter (fun point -> Queue.add (point, identity) queue) points; *)
(*         orbit_aux group queue IntMap.empty *)

(*       let of_cycles cycles = *)
(*         let cp = Concrete.of_cycles cycles in *)
(*         Perm { p = cp; invp = Concrete.inv cp } *)

(*       let print p = *)
(*         let perm = normalise_aux p in *)
(*         Concrete.print perm.p *)

(*       let print_orbit orb = *)
(*         let points = IntMap.bindings orb in *)
(*         Permtools.to_sseq (fun (point, transversal) -> *)
(*                          Printf.sprintf "%d with transversal:\n%s\n" point (print transversal) *)
(*                         ) "\n" points *)

(*       module Operators = *)
(*         struct *)

(*           let ( *** ) a b = Prod(a, b) *)

(*           let (^^) point perm = action perm point *)

(*         end *)

(*       let perm_test () = *)
(*         let open Operators in *)
(*         let alpha = of_cycles [ [| 0; 4; 1; 2 |] ] in *)

(*         let beta  = of_cycles [ [| 0; 4; 3 |]; [| 1; 2 |] ] in *)

(*         let ab = alpha *** beta in *)

(*         let _ = Printf.printf "alpha\n%s\n" (print alpha) in *)

(*         let _ = Printf.printf "%s\n" (print beta) in *)

(*         let _ = Printf.printf "%s\n" (print ab) in *)

(*         (\* Try some orbit computation *\) *)

(*         let orb = orbit [alpha] [1] in *)

(*         print_string (print_orbit orb) *)

(*     end : PermType) *)
