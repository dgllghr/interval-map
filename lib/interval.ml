module Make (Bound : Bound.S) = struct
  type t =
    { low : Bound.t
    ; high : Bound.t
    }

  let create low high = { low; high }

  let incl_incl low high = { low = Included low; high = Included high }

  let incl_excl low high = { low = Included low; high = Excluded high }

  let excl_incl low high = { low = Excluded low; high = Included high }

  let excl_excl low high = { low = Excluded low; high = Excluded high }

  let unbnd_incl high = { low = Unbounded; high = Included high }

  let unbnd_excl high = { low = Unbounded; high = Excluded high }

  let incl_unbnd low = { low = Included low; high = Unbounded }

  let excl_unbnd low = { low = Excluded low; high = Unbounded }

  let low_bound_compare a b = Bound.compare_lower a.low b.low

  let high_bound_compare a b = Bound.compare_upper a.high b.high

  let compare a b =
    let low_bound_cmp = low_bound_compare a b in
    if low_bound_cmp = 0 then
      high_bound_compare a b
    else
      low_bound_cmp

  let is_valid { low; high } =
    match low, high with
    | Included low, Included high ->
      low <= high
    | Included low, Excluded high
    | Excluded low, Included high
    | Excluded low, Excluded high ->
      low < high
    | _ ->
      true

  let get_overlap ivl_a ivl_b =
    let low =
      if low_bound_compare ivl_a ivl_b < 0 then
        ivl_b.low
      else
        ivl_a.low
    in
    let high =
      if high_bound_compare ivl_a ivl_b < 0 then
        ivl_a.high
      else
        ivl_b.high
    in
    let interval = { low; high } in
    if is_valid interval then
      Some interval
    else
      None

  let overlaps ivl_a ivl_b = get_overlap ivl_a ivl_b |> Option.is_some
end
