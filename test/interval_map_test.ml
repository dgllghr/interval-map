open Alcotest
open Interval_map
module Ivl_map = Make (Int)
module Ivl = Ivl_map.Interval

let create_and_add () =
  let map =
    Ivl_map.empty
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo"
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo2"
    |> Ivl_map.add (Ivl.create (Excluded 0) (Included 10)) "bar"
    |> Ivl_map.add (Ivl.create (Included 5) (Included 10)) "baz"
    |> Ivl_map.add (Ivl.create (Excluded 4) (Excluded 10)) "oof"
    |> Ivl_map.add (Ivl.create Unbounded (Excluded 4)) "zab"
  in
  check int "expected size" 6 (Ivl_map.size map)

let remove () =
  let map =
    Ivl_map.empty
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo"
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo2"
    |> Ivl_map.add (Ivl.create (Excluded 0) (Included 10)) "bar"
    |> Ivl_map.add (Ivl.create (Included 5) (Included 10)) "baz"
    |> Ivl_map.add (Ivl.create (Excluded 4) (Excluded 10)) "oof"
    |> Ivl_map.add (Ivl.create Unbounded (Excluded 4)) "zab"
  in
  let map =
    Ivl_map.remove_by
      (Ivl.create (Included 0) (Excluded 10))
      (fun v -> v = "foo")
      map
  in
  check int "expected size" 5 (Ivl_map.size map);
  let map =
    Ivl_map.remove_by
      (Ivl.create (Included 0) (Excluded 10))
      (fun v -> v = "foo2")
      map
  in
  check int "expected size" 4 (Ivl_map.size map);
  let map =
    Ivl_map.remove_by
      (Ivl.create (Excluded 0) (Included 10))
      (fun v -> v = "bar")
      map
  in
  check int "expected size" 3 (Ivl_map.size map);
  let map = Ivl_map.remove_interval (Ivl.create Unbounded (Excluded 4)) map in
  check int "expected size" 2 (Ivl_map.size map);
  let map =
    Ivl_map.remove_interval (Ivl.create (Excluded 4) (Excluded 10)) map
  in
  check int "expected size" 1 (Ivl_map.size map);
  let map =
    Ivl_map.remove_interval (Ivl.create (Included 5) (Included 10)) map
  in
  check int "expected size" 0 (Ivl_map.size map)

let query () =
  let module Ivl_map = Make (Int) in
  let module Ivl = Ivl_map.Interval in
  let open Ivl_map.Bound in
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
  (* build the map with random intervals *)
  let ivls = ref [] in
  let map = ref Ivl_map.empty in
  let c = ref 0 in
  while !c < 1000 do
    try
      let ivl = rand_ivl () in
      ivls := ivl :: !ivls;
      map := Ivl_map.add ivl (Random.int 10) !map;
      c := !c + 1
    with
    | Invalid_interval ->
      ()
  done;
  (* query the map with random intervals *)
  c := 0;
  while !c < 1000 do
    try
      let query = rand_ivl () in
      let expected_count =
        List.fold_left
          (fun acc ivl -> if Ivl.overlaps query ivl then acc + 1 else acc)
          0
          !ivls
      in
      let results_count =
        Ivl_map.query_interval query !map
        |> Ivl_map.Gen.fold (fun acc _ xs -> acc + List.length xs) 0
      in
      check int "same number of query results" expected_count results_count;
      c := !c + 1
    with
    | Invalid_interval ->
      ()
  done

let suite =
  [ "create and add", `Quick, create_and_add
  ; "remove", `Quick, remove
  ; "query", `Quick, query
  ]
