open Permgroup
module Perm = Perm.CycleBased (Permtools.Int)
module S = Group.Make (Perm)

let s5 =
  Perm.
    [
      of_cycles [[|0; 1|]];
      of_cycles [[|1; 2|]];
      of_cycles [[|2; 3|]];
      of_cycles [[|3; 4|]];
      of_cycles [[|4; 0|]];
    ]

let dih8 = Perm.[of_cycles [[|0; 1; 2; 3|]]; of_cycles [[|0; 2|]]]

let m11 =
  Perm.
    [
      of_cycles [[|1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11|]];
      of_cycles [[|3; 7; 11; 8|]; [|4; 10; 5; 6|]];
    ]

let a8 = Perm.[of_cycles [[|1; 2; 3|]]; of_cycles [[|2; 3; 4; 5; 6; 7; 8|]]]

let a13 =
  Perm.
    [
      of_mapping [(1, 2); (2, 3); (3, 1)];
      of_cycles [[|3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13|]];
    ]

let a14 =
  Perm.
    [
      of_cycles [[|1; 2; 3|]];
      of_cycles [[|2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14|]];
    ]

let generators = [s5; dih8; m11; a8; a13; a14]

let exact_orders =
  List.map Z.of_int [120; 8; 7920; 20160; 3113510400; 43589145600]

let computed_orders =
  generators |> List.map S.from_generators_mc |> List.map S.order

let _ =
  if List.for_all2 Z.equal computed_orders exact_orders then
    let _ = Printf.printf "Test passed." in
    exit 0
  else
    let _ = Printf.printf "Bug found! Please report." in
    exit 1
