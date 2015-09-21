module Perm = Perm.CycleBased(Tools.Int)
module S = SchreierSims.Make(Perm)

let s5 =
  Perm.(
    [
      of_cycles [ [| 0; 1 |] ];
      of_cycles [ [| 1; 2 |] ];
      of_cycles [ [| 2; 3 |] ];
      of_cycles [ [| 3; 4 |] ];
      of_cycles [ [| 4; 0 |] ];            
    ]
  )


let dih8 =
  Perm.(
    [
      of_cycles [ [| 0; 1; 2; 3 |] ];
      of_cycles [ [| 0; 2 |] ];
    ]
  )

let m11 =
  Perm.(
    [
      of_cycles [ [| 1;2;3;4;5;6;7;8;9;10;11 |] ];
      of_cycles [ [| 3;7;11;8 |]; [| 4;10;5;6 |] ]
    ]

  )

let a8 =
  Perm.(
    [
      of_cycles [ [| 1;2;3 |] ];
      of_cycles [ [| 2;3;4;5;6;7;8 |]; ]
    ]

  )

let a13 =
  Perm.(
    [
      (* of_cycles [ [| 1;2;3 |] ]; *)
      of_mapping [ (1, 2); (2, 3); (3, 1) ];
      of_cycles [ [| 3;4;5;6;7;8;9;10;11;12;13 |]; ]
    ]

  )
                 
let group = a13

let group = S.from_generators group

let _ = Printf.printf "order = %d\n" (S.order group)

(* let elements = S.list group *)

(* let _ = *)
(*   List.iter *)
(*     (fun p -> *)
(*      Printf.printf "%s\n%!" (Perm.print p) *)
(*     ) elements *)
