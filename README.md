# interval-tree

[![Actions Status](https://github.com/dgllghr/interval-tree/workflows/CI/badge.svg)](https://github.com/dgllghr/interval-tree/actions)

An immutable interval tree data structure. Interval trees are great for finding intervals which overlap a given interval.

This interval tree supports excluded, included, and unbounded bound ends.

## Installation

### Using Opam

```bash
opam install interval-tree
```

### Using Esy

```bash
esy add @opam/inquire
```

## Usage

### In OCaml

```ocaml
let module IT = Interval_tree.Make (Int) in
let module Ivl = IT.Interval in
(* Build the tree *)
let tree =
  IT.empty
  |> IT.insert (Ivl.create (Included 0) (Excluded 10)) "foo"
  |> IT.insert (Ivl.create (Included 0) (Excluded 10)) "foo2"
  |> IT.insert (Ivl.create (Excluded 0) (Included 10)) "bar"
  |> IT.insert (Ivl.create (Included 5) (Included 10)) "baz"
  |> IT.insert (Ivl.create (Excluded 4) (Excluded 10)) "oof"
  |> IT.insert (Ivl.create Unbounded (Excluded 4)) "zab"
in

(* Query the tree *)
let query = Ivl.create Unbounded (Included 4) in
IT.query_interval query tree
|> IT.Query_results.to_list
(* Results in:
  [({IT.Interval.low = IT.Bound.Unbounded; high = IT.Bound.Excluded 4},
    ["zab"]);
    ({IT.Interval.low = IT.Bound.Included 0; high = IT.Bound.Excluded 10},
    ["foo2", "foo"]);
    ({IT.Interval.low = IT.Bound.Excluded 0; high = IT.Bound.Included 10},
    ["bar"])]
  *)
```
