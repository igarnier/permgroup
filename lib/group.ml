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

  type info = {
    generators : perm list;
    stabilizers : elt list;
    indices : int list;
  }

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

(* The implementation of Schreier-Sims is functorialized over an implementation of permutations.  *)
module Make (Perm : Perm.S) = struct
  module Map = Map.Make (Perm.E)
  module Set = Set.Make (Perm.E)

  type transversal = Perm.t Map.t

  (* If base = b_1 ... b_k; we have subgroups
   * G = G^(1) >= G^(2) .... G^(k+1) = <1>
   * where G^(2) stabilises b_1, ... G^(k+1) = <1> stabilises b_k.
   * therefore, we have k transversals: G^(1) mod G^(2), G^(2) mod G^(3), .. G^(k) mod G^(k+1)
   * In what follows: cosets.(i) correspond to the transversal
   * of G^(i+1) mod G^(i+2), i.e. G^(i+1) mod Stab(base.(0) ... base.(i))
   * e.g.: cosets.(0) = G^(1) mod G^(2) = G^(1) mod Stab(base.(0))
   *)

  type perm = Perm.t

  type elt = Perm.E.t

  (* cosets of G mod Stab_{b_1,..,b_i}(G) *)
  type slice = {
    base : Perm.elt;
    cosets : transversal;
    reprs : Perm.t list;
    (* [reprs] = codomain of [cosets] *)
    gens : Perm.t list;
    (* strong generating set for the whole (sub)group *)
    reprs_checked : Perm.t list;
    gens_checked : Perm.t list;
    index : int; (* number of cosets w.r.t. the stabiliser subgroup *)
  }

  type t = slice list

  type info = {
    generators : perm list;
    stabilizers : elt list;
    indices : int list;
  }

  let info group =
    match group with
    | [] ->
        {generators = [Perm.identity]; stabilizers = []; indices = []}
    | slice :: _slices ->
        let generators = slice.gens
        and stabilizers = List.map (fun s -> s.base) group
        and indices = List.map (fun s -> s.index) group in
        {generators; stabilizers; indices}

  let pp_transversal fmtr tr =
    let bindings = Map.bindings tr in
    Format.pp_print_list
      ~pp_sep:(fun fmtr () -> Format.fprintf fmtr "@.")
      (fun fmtr (pt, repr) ->
        Format.fprintf fmtr "from base to %a by %a" Perm.E.pp pt Perm.pp repr)
      fmtr
      bindings

  let _pp fmtr {base; cosets; gens; _} =
    let open Format in
    fprintf fmtr "base = %a@." Perm.E.pp base ;
    fprintf fmtr "cosets =@." ;
    fprintf fmtr "%a@." pp_transversal cosets ;
    fprintf fmtr "gens=@." ;
    Format.pp_print_list
      ~pp_sep:(fun fmtr () -> Format.fprintf fmtr "@.")
      Perm.pp
      fmtr
      gens

  let trivial = []

  let rec mem group perm =
    match group with
    | [] ->
        Perm.equal perm Perm.identity
    | slice :: subgroup -> (
        let im = Perm.action perm slice.base in
        if Perm.E.equal im slice.base then mem subgroup perm
        else
          try
            let coset_p = Map.find im slice.cosets in
            let rem = Perm.prod perm (Perm.inv coset_p) in
            mem subgroup rem
          with Not_found -> false )

  let rec list_aux group current_word acc =
    match group with
    | [] ->
        current_word :: acc
    | slice :: subgroup ->
        Map.fold
          (fun _ elt acc -> list_aux subgroup (Perm.prod elt current_word) acc)
          slice.cosets
          acc

  let list group = list_aux group Perm.identity []

  let uniform group =
    let rec loop chain word =
      match chain with
      | [] ->
          word
      | slice :: tail ->
          let n = Random.int slice.index in
          let elt = List.nth slice.reprs n in
          loop tail (Perm.prod elt word)
    in
    loop group Perm.identity

  let rec order group =
    match group with
    | [] ->
        Z.one
    | slice :: tail ->
        Z.(mul (of_int slice.index) (order tail))

  (* ---------------------------------------------------------- *)
  (* Schreier-Sims construction *)

  let transversal_reprs transversal =
    Map.fold (fun _ elt acc -> elt :: acc) transversal []

  let rec transv_aux generators point coset_repr transversal =
    List.fold_left
      (fun transversal g ->
        let point' = Perm.action g point in
        if Map.mem point' transversal then transversal
        else
          let prod = Perm.prod coset_repr g in
          let transversal = Map.add point' prod transversal in
          transv_aux generators point' prod transversal)
      transversal
      generators

  let transv generators point =
    let transversal = Map.add point Perm.identity Map.empty in
    transv_aux generators point Perm.identity transversal

  let _orbit group point =
    match group with
    | [] ->
        Map.add point Perm.identity Map.empty
    | slice :: _ ->
        transv slice.gens point

  (* let rec pick_from_support generators =
   *   match generators with
   *   | [] -> None
   *   | g :: tail ->
   *     match Perm.pick_from_support g with
   *     | None -> pick_from_support tail
   *     | x    -> x *)

  let extend_transversal transversal group =
    Map.fold
      (fun point repr transversal -> transv_aux group point repr transversal)
      transversal
      transversal

  let rec extend subgroup_chain perm =
    match subgroup_chain with
    | [] -> (
      match Perm.pick_from_support perm with
      | None ->
          subgroup_chain
      | Some point ->
          let transversal = transv [perm] point in
          let reprs = transversal_reprs transversal in
          [
            {
              base = point;
              cosets = transversal;
              reprs;
              gens = reprs;
              reprs_checked = [];
              gens_checked = [];
              index = List.length reprs;
            };
          ] )
    | slice :: subgroup ->
        let cosets = extend_transversal slice.cosets (perm :: slice.gens) in
        let reprs = transversal_reprs cosets in
        let gens = perm :: slice.gens in
        let index = List.length reprs in
        let slice = {slice with cosets; reprs; gens; index} in
        let subgroup =
          let im = Perm.action perm slice.base in
          if Perm.E.equal im slice.base then extend subgroup perm
          else
            let coset_p = Map.find im slice.cosets in
            let rem = Perm.prod perm (Perm.inv coset_p) in
            extend subgroup rem
        in
        (* let gen_size   = (List.length slice.gens) - (List.length slice.gens_checked) in *)
        (* let coset_size = (List.length slice.reprs) - (List.length slice.reprs_checked) in *)
        (* let card       = float (gen_size * coset_size) in *)
        (* complete group by sifting schreier generators *)
        let (subgroup, reprs_explored, gens_explored) =
          Permtools.fold_cartesian
            (fun coset_repr
                 generator
                 ((subgroup, reprs_explored, gens_explored) as acc) ->
              if
                List.exists (fun p -> p == coset_repr) reprs_explored
                && List.exists (fun p -> p == generator) gens_explored
              then acc
              else
                (* if (Random.float card) > (card *. 0.2) then acc *)
                (* else *)
                let p = Perm.prod coset_repr generator in
                let r = Map.find (Perm.action p slice.base) slice.cosets in
                let subgroup = extend subgroup (Perm.prod p (Perm.inv r)) in
                ( subgroup,
                  coset_repr :: reprs_explored,
                  generator :: gens_explored ))
            slice.reprs
            slice.gens
            (subgroup, slice.reprs_checked, slice.gens_checked)
        in
        let slice =
          {
            slice with
            reprs_checked = reprs_explored;
            gens_checked = gens_explored;
          }
        in
        slice :: subgroup

  let rec extend_mc subgroup_chain perm =
    match subgroup_chain with
    | [] -> (
      match Perm.pick_from_support perm with
      | None ->
          subgroup_chain
      | Some point ->
          let transversal = transv [perm] point in
          let reprs = transversal_reprs transversal in
          [
            {
              base = point;
              cosets = transversal;
              reprs;
              gens = reprs;
              reprs_checked = [];
              gens_checked = [];
              index = List.length reprs;
            };
          ] )
    | slice :: subgroup ->
        let cosets = extend_transversal slice.cosets (perm :: slice.gens) in
        let reprs = transversal_reprs cosets in
        let gens = perm :: slice.gens in
        let index = List.length reprs in
        let slice = {slice with cosets; reprs; gens; index} in
        let subgroup =
          let im = Perm.action perm slice.base in
          if Perm.E.equal im slice.base then extend_mc subgroup perm
          else
            let coset_p = Map.find im slice.cosets in
            let rem = Perm.prod perm (Perm.inv coset_p) in
            extend_mc subgroup rem
        in
        (* complete group by sifting schreier generators *)
        let (subgroup, reprs_explored, gens_explored, _) =
          Permtools.fold_cartesian
            (fun coset_repr
                 generator
                 ((subgroup, reprs_explored, gens_explored, consecutive) as acc)
                 ->
              if
                consecutive = -1
                || List.exists (fun p -> p == coset_repr) reprs_explored
                   && List.exists (fun p -> p == generator) gens_explored
              then acc
              else
                let p = Perm.prod coset_repr generator in
                let r = Map.find (Perm.action p slice.base) slice.cosets in
                let schreier = Perm.prod p (Perm.inv r) in
                if mem subgroup schreier then
                  if consecutive = 49 then
                    ( subgroup,
                      coset_repr :: reprs_explored,
                      generator :: gens_explored,
                      -1 )
                  else
                    ( subgroup,
                      coset_repr :: reprs_explored,
                      generator :: gens_explored,
                      consecutive + 1 )
                else
                  let subgroup = extend_mc subgroup schreier in
                  ( subgroup,
                    coset_repr :: reprs_explored,
                    generator :: gens_explored,
                    0 ))
            slice.reprs
            slice.gens
            (subgroup, slice.reprs_checked, slice.gens_checked, 0)
        in
        let slice =
          {
            slice with
            reprs_checked = reprs_explored;
            gens_checked = gens_explored;
          }
        in
        slice :: subgroup

  let from_generators gens = List.fold_left extend [] gens

  let from_generators_mc gens = List.fold_left extend_mc [] gens
end
