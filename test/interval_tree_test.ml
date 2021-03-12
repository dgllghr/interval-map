open Alcotest
open Interval_tree

let create_and_insert () =
  let module IT = Make (Int) in
  let module Ivl = IT.Interval in
  let t =
    IT.empty
    |> IT.insert (Ivl.incl_excl 0 10) "foo"
    |> IT.insert (Ivl.excl_incl 0 10) "bar"
    |> IT.insert (Ivl.incl_incl 5 10) "baz"
    |> IT.insert (Ivl.excl_excl 5 10) "oof"
    |> IT.insert (Ivl.unbnd_incl 5) "rab"
  in
  check int "expected size" 5 (IT.size t)

let query () =
  let module IT = Make (Int) in
  let module Ivl = IT.Interval in
  let open IT.Bound in
  Random.self_init ();
  let rand_bound bound_value =
    let bound_roll = Random.int 100 in
    if bound_roll >= 90 then
      Unbounded
    else if bound_roll >= 45 then
      Excluded bound_value
    else
      Included bound_value
  in
  let rand_ivl () =
    let bv1 = Random.int 100 in
    let bv2 = bv1 + Random.int 20 - 10 in
    let b1 = rand_bound bv1 in
    let b2 = rand_bound bv2 in
    let low, high =
      if compare_lower b1 b2 <= 0 then
        b1, b2
      else
        b2, b1
    in
    Ivl.create low high
  in
  (* build the tree with random intervals *)
  let ivls = ref [] in
  let tree = ref IT.empty in
  let c = ref 0 in
  while !c < 1000 do
    let ivl = rand_ivl () in
    ivls := ivl :: !ivls;
    tree := IT.insert ivl (Random.int 10) !tree;
    c := !c + 1
  done;
  (* query the tree with random intervals *)
  c := 0;
  while !c < 1000 do
    let query = rand_ivl () in
    let expected_count =
      List.fold_left
        (fun acc ivl -> if Ivl.overlaps query ivl then acc + 1 else acc)
        0
        !ivls
    in
    let results_count =
      IT.query_interval query !tree
      |> IT.Query_results.fold (fun acc (_, xs) -> acc + List.length xs) 0
    in
    check int "same number of query results" expected_count results_count;
    c := !c + 1
  done;
  flush stderr

let suite =
  [ "create and insert", `Quick, create_and_insert; "query", `Quick, query ]
