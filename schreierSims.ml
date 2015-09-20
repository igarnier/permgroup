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
module type S =
  sig

    type t

    type perm

    (* Accessing the group *)

    val mem : t -> perm -> bool

    val list : t -> perm list

    val uniform : t -> perm

    val order : t -> int

    (* Building the group *)

    val extend : t -> perm -> t

    val from_generators : perm list -> t
                       
  end

(* The implementation of Schreier-Sims is functorialized over an implementation of permutations.  *)    
module Make(Perm : Perm.S) : S with type perm = Perm.t =
  struct

    open Tools
    
    type transversal = Perm.t IntMap.t

    (* If base = b_1 ... b_k; we have subgroups
     * G = G^(1) >= G^(2) .... G^(k+1) = <1>
     * where G^(2) stabilises b_1, ... G^(k+1) = <1> stabilises b_k.
     * therefore, we have k transversals: G^(1) mod G^(2), G^(2) mod G^(3), .. G^(k) mod G^(k+1)
     * In what follows: cosets.(i) correspond to the transversal 
     * of G^(i+1) mod G^(i+2), i.e. G^(i+1) mod Stab(base.(0) ... base.(i))
     * e.g.: cosets.(0) = G^(1) mod G^(2) = G^(1) mod Stab(base.(0))
     *)
                              
    type perm = Perm.t
                              
    (* cosets of G mod Stab_{b_1,..,b_i}(G) *)                              
    type slice =
      {
        base          : int;
        cosets        : transversal;
        reprs         : Perm.t list;    (* [reprs] = codomain of [cosets] *)
        gens          : Perm.t list;    (* strong generating set for the whole (sub)group *)
        reprs_checked : Perm.t list;
        gens_checked  : Perm.t list;        
        index         : int             (* number of cosets w.r.t. the stabiliser subgroup *)
      }

    type t = slice list

    let print_transversal tr =
      IntMap.fold (fun pt repr acc -> acc^(Printf.sprintf "from base to %d by %s\n") pt (Perm.print repr)) tr ""
                   
    let print { base; cosets; gens } =
      Printf.printf
        "base = %d\ncosets =\n %s\ngens=\n%s\n-----------------------------\n"
        base
        (print_transversal cosets)
        (List.fold_left (fun acc perm -> acc^(Printf.sprintf "%s\n" (Perm.print perm))) "" gens)
                   

    let rec mem group perm =
      match group with
      | [] ->
         Perm.equal perm Perm.identity
      | slice :: subgroup ->
         let im = Perm.action perm slice.base in
         if im = slice.base then
           mem subgroup perm
         else
           try 
             let coset_p = IntMap.find im slice.cosets in
             let rem     = Perm.prod perm (Perm.inv coset_p) in
             mem subgroup rem
           with
           | Not_found -> false

    let rec list_aux group current_word acc =
      match group with
      | []                -> current_word :: acc
      | slice :: subgroup -> 
         IntMap.fold
           (fun _ elt acc ->
            list_aux subgroup (Perm.prod elt current_word) acc
           ) slice.cosets acc

    let list group = list_aux group Perm.identity []

    let uniform group =
      let rec loop chain word =
        match chain with
        | [] -> word
        | slice :: tail ->
           let n   = Random.int slice.index in
           let elt = List.nth slice.reprs n in
           loop tail (Perm.prod elt word)
      in
      loop group Perm.identity

    let rec order group =
      match group with
      | [] -> 1
      | slice :: tail ->
         slice.index * (order tail)
                         
    (* ---------------------------------------------------------- *)
    (* Schreier-Sims construction *)

    let transversal_reprs transversal =
      IntMap.fold (fun _ elt acc -> elt :: acc) transversal []

    let rec orbit_aux group point coset_repr transversal =
      List.fold_left (fun transversal g ->
                      let point' = Perm.action g point in
                      if IntMap.mem point' transversal then
                        transversal
                      else
                        let prod        = Perm.prod coset_repr g in
                        let transversal = IntMap.add point' prod transversal in
                        orbit_aux group point' prod transversal
                     ) transversal group

    let orbit group point =
      let transversal = IntMap.add point Perm.identity IntMap.empty in
      orbit_aux group point Perm.identity transversal

    let rec pick_from_support generators =
      match generators with
      | [] -> None
      | g :: tail ->
         match Perm.pick_from_support g with
         | None -> pick_from_support tail
         | x    -> x

    let extend_transversal transversal group =
      IntMap.fold (fun point repr transversal -> orbit_aux group point repr transversal) transversal transversal

    let rec extend subgroup_chain perm =
      match subgroup_chain with
      | [] ->
         (match Perm.pick_from_support perm with
          | None -> 
             subgroup_chain
          | Some point ->
             let transversal = orbit [perm] point in
             let reprs       = transversal_reprs transversal in
             {
               base          = point;
               cosets        = transversal;
               reprs         = reprs;
               gens          = reprs;
               reprs_checked = [];
               gens_checked  = [];
               index         = List.length reprs
             } :: []
         )
      | slice :: subgroup ->
         let slice' = slice in
         let cosets = extend_transversal slice.cosets (perm :: slice.gens) in
         let reprs  = transversal_reprs cosets in
         let gens   = perm :: slice.gens in
         let index  = List.length reprs in
         let slice  = { slice with cosets; reprs; gens; index } in
         let subgroup =
           let im = Perm.action perm slice.base in
           if im = slice.base then
             extend subgroup perm
           else
             let coset_p = IntMap.find im slice.cosets in
             let rem     = Perm.prod perm (Perm.inv coset_p) in
             extend subgroup rem
         in
         let (subgroup, reprs_explored, gens_explored) =
           Tools.fold_cartesian
             (fun coset_repr generator ((subgroup, reprs_explored, gens_explored) as acc) ->
              if List.exists (fun p -> Perm.equal p coset_repr) reprs_explored &&
                 List.exists (fun p -> Perm.equal p generator) gens_explored
              then
                acc
              else
                let p = Perm.prod coset_repr generator in
                let r =
                  try IntMap.find (Perm.action p slice.base) slice.cosets
                  with Not_found ->
                       begin
                         let s = Perm.print p in
                         Printf.printf "perm=\n%s\nbase = %d\nschreier =\n %s\ntransversal=%s\n" (Perm.print perm) slice.base s (print_transversal slice'.cosets);
                         raise Not_found
                       end
                in
                let subgroup =  extend subgroup (Perm.prod p (Perm.inv r)) in
                (subgroup, coset_repr :: reprs_explored, generator :: gens_explored)
             )
             slice.reprs slice.gens (subgroup, slice.reprs_checked, slice.gens_checked)
         in
         let slice = { slice with reprs_checked = reprs_explored; gens_checked = gens_explored } in
         slice :: subgroup
               
    let from_generators gens =
      List.fold_left extend [] gens 

  end
