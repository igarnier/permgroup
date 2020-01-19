open Permgroup
module P = Perm.Hash_consed (Perm.Cycle_based (Permtools.Int))
module S = Group.Make (P)

let s5 =
  P.
    [
      of_cycles [[|0; 1|]];
      of_cycles [[|1; 2|]];
      of_cycles [[|2; 3|]];
      of_cycles [[|3; 4|]];
      of_cycles [[|4; 0|]];
    ]

let dih8 = P.[of_cycles [[|0; 1; 2; 3|]]; of_cycles [[|0; 2|]]]

let m11 =
  P.
    [
      of_cycles [[|1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11|]];
      of_cycles [[|3; 7; 11; 8|]; [|4; 10; 5; 6|]];
    ]

let m24 =
  P.
    [
      of_cycles [Array.init 23 (fun i -> i + 1)];
      of_cycles
        [
          [|3; 17; 10; 7; 9|];
          [|4; 13; 14; 19; 5|];
          [|8; 18; 11; 12; 23|];
          [|15; 20; 22; 21; 16|];
        ];
      of_cycles
        [
          [|1; 24|];
          [|2; 23|];
          [|3; 12|];
          [|4; 16|];
          [|5; 18|];
          [|6; 10|];
          [|7; 20|];
          [|8; 14|];
          [|9; 21|];
          [|11; 17|];
          [|13; 22|];
          [|15; 19|];
        ];
    ]

let a8 = P.[of_cycles [[|1; 2; 3|]]; of_cycles [[|2; 3; 4; 5; 6; 7; 8|]]]

let a13 =
  P.
    [
      of_mapping [(1, 2); (2, 3); (3, 1)];
      of_cycles [[|3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13|]];
    ]

let a14 =
  P.
    [
      of_cycles [[|1; 2; 3|]];
      of_cycles [[|2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14|]];
    ]

let a14_ = List.init (14 - 2) (fun i -> P.of_cycles [[|i; i + 1; i + 2|]])

let generators = [s5; dih8; m11; a8; a13; a14; m24]

let t = Unix.gettimeofday ()

let exact_orders =
  List.map Z.of_int [120; 8; 7920; 20160; 3113510400; 43589145600; 244823040]

let computed_orders =
  generators |> List.map S.from_generators |> List.map S.order

let t' = Unix.gettimeofday ()

let _ =
  if List.for_all2 Z.equal computed_orders exact_orders then
    Printf.printf "Test passed (%f)." (t' -. t)
  else (
    Printf.printf "Bug found! Please report." ;
    exit 1 )
