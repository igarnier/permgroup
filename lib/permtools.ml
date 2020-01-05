module type Comparable = sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val to_string : t -> string
end

module type ComparableAndHashable = sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  (* invariant: hash should be invariant under [equal] *)
  val hash : t -> int

  val to_string : t -> string
end

module Int = struct
  type t = int

  let compare (x : int) (y : int) =
    if x < y then -1 else if x = y then 0 else 1

  let equal (x : int) (y : int) = x = y

  let to_string = string_of_int
end

module IntSet = Set.Make (Int)

module IntMap = struct
  include Map.Make (Int)

  let find_opt k m = try Some (find k m) with Not_found -> None
end

let to_sseq f sep l =
  match l with
  | [] ->
      ""
  | [x] ->
      f x
  | x :: tl ->
      let res = List.fold_right (fun elt acc -> sep ^ f elt ^ acc) tl "" in
      f x ^ res

let strof_ilist = to_sseq string_of_int ","

let strof_iarr x = strof_ilist (Array.to_list x)

(* duplicate an elt i times *)
let rec dup elt i =
  if i < 0 then failwith "dup: count < 0"
  else if i = 0 then []
  else elt :: dup elt (i - 1)

(* list of integers from i to j *)
let rec mk_ints i j = if i > j then [] else i :: mk_ints (i + 1) j

(* fold over a cartesian product *)
let rec fold_cartesian f l1 l2 l2' acc =
  match l1 with
  | [] ->
      acc
  | x1 :: tl1 -> (
    match l2' with
    | [] ->
        fold_cartesian f tl1 l2 l2 acc
    | x2 :: tl2 ->
        let acc = f x1 x2 acc in
        fold_cartesian f l1 l2 tl2 acc )

let fold_cartesian f l1 l2 acc = fold_cartesian f l1 l2 l2 acc
