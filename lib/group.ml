(*
 * Schreier-Sims algorithm implementation.
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

(* Schreier-Sims perm group signature *)
module type S = sig
  type t

  type perm

  type elt

  type info =
    { generators : perm list; stabilizers : elt list; indices : int list }

  val info : t -> info

  val trivial : t

  val mem : t -> perm -> bool

  val list : t -> perm list

  val uniform : t -> perm

  val order : t -> Z.t

  val extend : t -> perm -> t

  val extend_mc : t -> perm -> t

  val from_generators : perm list -> t

  val from_generators_mc : perm list -> t
end

module Make (E : Perm.Element_sig) = struct
  module Perm = Perm.Hash_consed (Perm.Cycle_based (E))
  module EMap = Map.Make (Perm.E)
  module ESet = Set.Make (Perm.E)

  type perm = Perm.t

  type elt = E.t

  module Perm_set = Set.Make (Perm)

  module Hash_set_pair = struct
    module H = Hashtbl.Make (struct
      type t = Perm.t * Perm.t

      let hash (p, q) = Hashtbl.hash (Perm.hash p, Perm.hash q)

      let equal (p, q) (p', q') = Perm.equal p p' && Perm.equal q q'
    end)

    type t = unit H.t

    let create () = H.create 41

    let add set perm_pair = H.add set perm_pair ()

    let mem set perm_pair = H.mem set perm_pair
  end

  type t = slice list

  and slice =
    { base : Perm.elt;
      cosets : transversal;
      reprs : Perm_set.t;
      (* [reprs] = codomain of [cosets] *)
      gens : Perm_set.t;
      (* strong generating set for the whole (sub)group *)
      schreier_checked : Hash_set_pair.t;
      index : int (* number of cosets w.r.t. the stabiliser subgroup *)
    }

  and transversal = Perm.t EMap.t

  type info =
    { generators : perm list; stabilizers : elt list; indices : int list }

  let info group =
    match group with
    | [] -> { generators = [Perm.identity]; stabilizers = []; indices = [] }
    | slice :: _slices ->
        let generators = Perm_set.elements slice.gens
        and stabilizers = List.map (fun s -> s.base) group
        and indices = List.map (fun s -> s.index) group in
        { generators; stabilizers; indices }

  let pp_transversal fmtr tr =
    let bindings = EMap.bindings tr in
    Format.pp_print_list
      ~pp_sep:(fun fmtr () -> Format.fprintf fmtr "@.")
      (fun fmtr (pt, repr) ->
        Format.fprintf fmtr "from base to %a by %a" Perm.E.pp pt Perm.pp repr)
      fmtr
      bindings

  let _pp fmtr { base; cosets; gens; _ } =
    let open Format in
    fprintf fmtr "base = %a@." Perm.E.pp base ;
    fprintf fmtr "cosets =@." ;
    fprintf fmtr "%a@." pp_transversal cosets ;
    fprintf fmtr "gens=@." ;
    Format.pp_print_list
      ~pp_sep:(fun fmtr () -> Format.fprintf fmtr "@.")
      Perm.pp
      fmtr
      (Perm_set.elements gens)

  let trivial = []

  (* Check whether a permutation belongs to the group. *)
  let rec mem group perm =
    match group with
    | [] -> Perm.equal perm Perm.identity
    | slice :: subgroup -> (
        let im = Perm.action perm slice.base in
        if Perm.E.equal im slice.base then mem subgroup perm
        else
          try
            let coset_p = EMap.find im slice.cosets in
            let rem = Perm.prod perm (Perm.inv coset_p) in
            mem subgroup rem
          with Not_found -> false)

  (* List all group elements *)
  let rec list group = list_aux group Perm.identity []

  and list_aux group current_word acc =
    match group with
    | [] -> current_word :: acc
    | slice :: subgroup ->
        EMap.fold
          (fun _ elt acc -> list_aux subgroup (Perm.prod elt current_word) acc)
          slice.cosets
          acc

  (* Uniform sampling of group elements *)
  let uniform group =
    let rec loop chain word =
      match chain with
      | [] -> word
      | slice :: tail ->
          let n = Random.int slice.index in
          let elt = List.nth (Perm_set.elements slice.reprs) n in
          loop tail (Perm.prod elt word)
    in
    loop group Perm.identity

  (* Order of a group *)
  let rec order group =
    match group with
    | [] -> Z.one
    | slice :: tail -> Z.(mul (of_int slice.index) (order tail))

  (* ---------------------------------------------------------- *)
  (* Schreier-Sims construction *)

  let transversal_reprs (transversal : transversal) : Perm_set.t =
    EMap.fold
      (fun _ perm acc -> Perm_set.add perm acc)
      transversal
      Perm_set.empty

  (* Compute a transversal for the group generated by [generator] modulo the
     subgroup stabilising [point]. *)
  let rec transv (generators : Perm_set.t) (point : elt) : transversal =
    let transversal = EMap.add point Perm.identity EMap.empty in
    transv_aux generators point Perm.identity transversal

  and transv_aux (generators : Perm_set.t) (point : elt) (coset_repr : perm)
      (transversal : transversal) : transversal =
    Perm_set.fold
      (fun g transversal ->
        let point' = Perm.action g point in
        if EMap.mem point' transversal then transversal
        else
          let prod = Perm.prod coset_repr g in
          let transversal = EMap.add point' prod transversal in
          transv_aux generators point' prod transversal)
      generators
      transversal

  let _orbit group point =
    match group with
    | [] -> EMap.add point Perm.identity EMap.empty
    | slice :: _ -> transv slice.gens point

  (* Extend a transversal with new group elements. It is expected
     that [group] is a superset of the group elements used to
     generate [transversal] *)
  let extend_transversal (transversal : transversal) (generators : Perm_set.t) :
      transversal =
    EMap.fold
      (fun point repr transversal ->
        transv_aux generators point repr transversal)
      transversal
      transversal

  let fold_cartesian_set f s1 s2 acc =
    Perm_set.fold
      (fun x acc -> Perm_set.fold (fun y acc -> f x y acc) s2 acc)
      s1
      acc

  (* Extend a subgroup chain with a new permutation *)
  let rec extend subgroup_chain perm =
    match subgroup_chain with
    | [] -> (
        match Perm.pick_from_support perm with
        | None -> subgroup_chain
        | Some point ->
            let transversal = transv (Perm_set.singleton perm) point in
            let reprs = transversal_reprs transversal in
            [ { base = point;
                cosets = transversal;
                reprs;
                gens = reprs;
                schreier_checked = Hash_set_pair.create ();
                index = Perm_set.cardinal reprs
              } ])
    | slice :: subgroup ->
        let gens = Perm_set.add perm slice.gens in
        let cosets = extend_transversal slice.cosets gens in
        let reprs = transversal_reprs cosets in
        let old_gens = slice.gens in
        let new_gens = Perm_set.singleton perm in
        let old_reprs = slice.reprs in
        let new_reprs = Perm_set.diff reprs slice.reprs in
        let index = Perm_set.cardinal reprs in
        let slice = { slice with cosets; reprs; gens; index } in
        (* Recursively extend subgroups.
           Invariant: [subgroup] is a well-formed stabilizer chain *)
        let subgroup =
          let im = Perm.action perm slice.base in
          if Perm.E.equal im slice.base then extend subgroup perm
          else
            let coset_p = EMap.find im slice.cosets in
            let rem = Perm.prod perm (Perm.inv coset_p) in
            if mem subgroup rem then subgroup else extend subgroup rem
        in
        (* complete group by sifting schreier generators *)
        let subgroup = extend_schreier slice new_reprs old_gens subgroup in
        let subgroup = extend_schreier slice old_reprs new_gens subgroup in
        let subgroup = extend_schreier slice new_reprs new_gens subgroup in
        slice :: subgroup

  and extend_schreier slice reprs gens subgroup =
    fold_cartesian_set
      (fun coset_repr generator subgroup ->
        if Hash_set_pair.mem slice.schreier_checked (coset_repr, generator) then
          subgroup
        else
          let p = Perm.prod coset_repr generator in
          let r = EMap.find (Perm.action p slice.base) slice.cosets in
          Hash_set_pair.add slice.schreier_checked (coset_repr, generator) ;
          let schreier = Perm.prod p (Perm.inv r) in
          if mem subgroup schreier then subgroup else extend subgroup schreier)
      reprs
      gens
      subgroup

  (* Extend a subgroup chain with a new permutation *)
  [@@@warning "-32"]

  let rec extend_mc subgroup_chain perm =
    match subgroup_chain with
    | [] -> (
        match Perm.pick_from_support perm with
        | None -> subgroup_chain
        | Some point ->
            let transversal = transv (Perm_set.singleton perm) point in
            let reprs = transversal_reprs transversal in
            [ { base = point;
                cosets = transversal;
                reprs;
                gens = reprs;
                schreier_checked = Hash_set_pair.create ();
                index = Perm_set.cardinal reprs
              } ])
    | slice :: subgroup ->
        let gens = Perm_set.add perm slice.gens in
        let cosets = extend_transversal slice.cosets gens in
        let reprs = transversal_reprs cosets in
        let index = Perm_set.cardinal reprs in
        let new_gens = Perm_set.diff gens slice.gens in
        let new_reprs = Perm_set.diff reprs slice.reprs in
        let slice = { slice with cosets; reprs; gens; index } in
        let subgroup =
          let im = Perm.action perm slice.base in
          if Perm.E.equal im slice.base then extend_mc subgroup perm
          else
            let coset_p = EMap.find im slice.cosets in
            let rem = Perm.prod perm (Perm.inv coset_p) in
            if mem subgroup rem then subgroup else extend_mc subgroup rem
        in
        (* complete group by sifting schreier generators *)
        let subgroup =
          Permtools.fold_cartesian
            (fun coset_repr generator subgroup ->
              if Hash_set_pair.mem slice.schreier_checked (coset_repr, generator)
              then subgroup
              else
                let p = Perm.prod coset_repr generator in
                let r = EMap.find (Perm.action p slice.base) slice.cosets in
                let schreier = Perm.prod p (Perm.inv r) in
                Hash_set_pair.add slice.schreier_checked (coset_repr, generator) ;
                if mem subgroup schreier then subgroup
                else extend_mc subgroup schreier)
            (Perm_set.elements new_reprs)
            (Perm_set.elements new_gens)
            subgroup
        in
        slice :: subgroup

  let from_generators gens = List.fold_left extend [] gens

  let from_generators_mc gens = List.fold_left extend_mc [] gens
end
