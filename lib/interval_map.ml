module type Comparable = Comparable.S

exception Invalid_interval = Interval.Invalid_interval

module Make (Bound_compare : Comparable) = struct
  module Bound = Bound.Make (Bound_compare)
  module Interval = Interval.Make (Bound)

  module Node = struct
    type 'v t =
      { interval : Interval.t
      ; left : 'v t option
      ; right : 'v t option
      ; height : int
      ; max : Bound.t
      ; min : Bound.t
      ; values : 'v list
      }

    let height node = match node with None -> 0 | Some n -> n.height

    let max_bound (interval : Interval.t) left right =
      let mid = interval.high in
      match left, right with
      | None, None ->
        mid
      | None, Some r ->
        Bound.max_upper mid r.max
      | Some l, None ->
        Bound.max_upper mid l.max
      | Some l, Some r ->
        Bound.max_upper mid (Bound.max_upper l.max r.max)

    let min_bound (interval : Interval.t) left right =
      let mid = interval.low in
      match left, right with
      | None, None ->
        mid
      | None, Some r ->
        Bound.min_lower mid r.min
      | Some l, None ->
        Bound.min_lower mid l.min
      | Some l, Some r ->
        Bound.min_lower mid (Bound.min_lower l.min r.min)

    let create interval left right values =
      let height = max (height left) (height right) + 1 in
      let max = max_bound interval left right in
      let min = min_bound interval left right in
      { interval; left; right; height; max; min; values }

    let leaf interval values = create interval None None values

    let size node =
      let rec go { left; right; values; _ } size =
        let size = size + List.length values in
        match left, right with
        | None, None ->
          size
        | Some lf, None ->
          go lf size
        | None, Some rt ->
          go rt size
        | Some lf, Some rt ->
          let size = go lf size in
          go rt size
      in
      go node 0

    let rec find interval node =
      let ivl_cmp = Interval.compare interval node.interval in
      if ivl_cmp = 0 then
        Some node
      else if ivl_cmp < 0 then
        match node.left with None -> None | Some lt -> find interval lt
      else
        match node.right with None -> None | Some rt -> find interval rt

    let balance_factor { left; right; _ } = height left - height right

    let replace_left node new_left =
      create node.interval new_left node.right node.values

    let replace_right node new_right =
      create node.interval node.left new_right node.values

    let rotate_right node =
      let pivot = Option.get node.left in
      let new_right = replace_left node pivot.right in
      replace_right pivot (Some new_right)

    let rotate_left node =
      let pivot = Option.get node.right in
      let new_left = replace_right node pivot.left in
      replace_left pivot (Some new_left)

    let balance node =
      let bf = balance_factor node in
      if bf < -1 then
        let right = Option.get node.right in
        if balance_factor right > 0 then
          replace_right node (Some (rotate_right right)) |> rotate_left
        else
          rotate_left node
      else if bf > 1 then
        let left = Option.get node.left in
        if balance_factor left < 0 then
          replace_left node (Some (rotate_left left)) |> rotate_right
        else
          rotate_right node
      else
        node

    (* Not tail recursive! *)
    let rec insert interval value node =
      let ivl_cmp = Interval.compare interval node.interval in
      let res =
        if ivl_cmp < 0 then
          let insert_left =
            match node.left with
            | None ->
              leaf interval [ value ]
            | Some lt ->
              insert interval value lt
          in
          replace_left node (Some insert_left)
        else if ivl_cmp > 0 then
          let insert_right =
            match node.right with
            | None ->
              leaf interval [ value ]
            | Some rt ->
              insert interval value rt
          in
          replace_right node (Some insert_right)
        else
          { node with values = value :: node.values }
      in
      balance res

    let rec min_interval node =
      match node.left with None -> node.interval | Some lt -> min_interval lt

    (* Not tail recursive! *)
    let rec remove_by interval value_rm_fn node =
      let ivl_cmp = Interval.compare interval node.interval in
      let res =
        if ivl_cmp = 0 then
          let values = List.filter (fun v -> not (value_rm_fn v)) node.values in
          match values, node.left, node.right with
          | _ :: _, _, _ ->
            Some { node with values }
          | _, None, None ->
            None
          | _, Some lt, None ->
            Some lt
          | _, None, Some rt ->
            Some rt
          | _, Some _, Some rt ->
            let successor = min_interval rt in
            let rt = remove_by interval value_rm_fn rt in
            let node = create successor node.left rt values in
            Some node
        else if ivl_cmp < 0 then
          match node.left with
          | None ->
            Some node
          | Some lt ->
            let lt = remove_by interval value_rm_fn lt in
            Some (replace_left node lt)
        else
          match node.right with
          | None ->
            Some node
          | Some rt ->
            let rt = remove_by interval value_rm_fn rt in
            Some (replace_right node rt)
      in
      match res with None -> None | Some n -> Some (balance n)
  end

  module Query_results = struct
    open Node

    type 'v t =
      { stack : 'v Node.t list
      ; query : Interval.t
      }

    let create interval = function
      | None ->
        { stack = []; query = interval }
      | Some n ->
        { stack = [ n ]; query = interval }

    let rec next { stack; query } =
      match stack with
      | node :: stack ->
        let stack = ref stack in
        Option.iter
          (fun (left : 'v Node.t) ->
            let max_is_gte =
              match left.max, query.low with
              | Included max, Included low ->
                max >= low
              | Included max, Excluded low
              | Excluded max, Included low
              | Excluded max, Excluded low ->
                max > low
              | _ ->
                true
            in
            if max_is_gte then
              stack := left :: !stack
            else
              ())
          node.left;
        Option.iter
          (fun (right : 'v Node.t) ->
            let min_is_lte =
              match right.min, query.high with
              | Included min, Included high ->
                min <= high
              | Included min, Excluded high
              | Excluded min, Included high
              | Excluded min, Excluded high ->
                min < high
              | _ ->
                true
            in
            if min_is_lte then
              stack := right :: !stack
            else
              ())
          node.right;
        if Interval.overlaps query node.interval then
          Some ((node.interval, node.values), { stack = !stack; query })
        else
          next { stack = !stack; query }
      | _ ->
        None

    let rec fold fn acc gen =
      match next gen with
      | None ->
        acc
      | Some (v, gen) ->
        let acc = fn acc v in
        fold fn acc gen

    let to_list gen = fold (fun acc v -> v :: acc) [] gen
  end

  type 'v t = { root : 'v Node.t option }

  let empty = { root = None }

  let size { root } = match root with None -> 0 | Some n -> Node.size n

  let cardinal = size

  let add interval value { root } =
    let new_root =
      match root with
      | None ->
        Node.leaf interval [ value ]
      | Some n ->
        Node.insert interval value n
    in
    { root = Some new_root }

  let find_opt interval { root } =
    let open Node in
    match root with
    | None ->
      None
    | Some n ->
      Node.find interval n |> Option.map (fun n -> n.values)

  let find interval map =
    match find_opt interval map with None -> raise Not_found | Some v -> v

  let mem interval { root } =
    match root with
    | None ->
      false
    | Some n ->
      Node.find interval n |> Option.is_some

  let query_interval interval { root } = Query_results.create interval root

  let query_interval_list interval { root } =
    Query_results.create interval root |> Query_results.to_list

  let remove_by interval value_rm_fn map =
    match map.root with
    | None ->
      map
    | Some n ->
      { root = Node.remove_by interval value_rm_fn n }

  let remove_interval interval map = remove_by interval (fun _ -> true) map
end
