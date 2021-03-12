# interval-tree

[![Actions Status](https://github.com/Dan/interval-tree/workflows/CI/badge.svg)](https://github.com/Dan/interval-tree/actions)

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
  |> IT.insert (Ivl.incl_excl 0 10) "foo"
  |> IT.insert (Ivl.incl_excl 0 10) "foo2"
  |> IT.insert (Ivl.excl_incl 0 10) "bar"
  |> IT.insert (Ivl.incl_incl 5 10) "baz"
  |> IT.insert (Ivl.excl_excl 4 10) "oof"
  |> IT.insert (Ivl.unbnd_excl 4) "zab"
in

(* Query the tree *)
let query = Ivl.unbnd_incl 4 in
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
