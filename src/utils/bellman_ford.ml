type 'node edge = 'node * 'node * int

module Make (Node : Baby.OrderedType) = struct
  type tbl_key = Node.t

  type tbl_entry = int option * Node.t edge option
  (** 2-tuple (distance, predecessor) has current shortest DISTANCE
      with the corresponding PREDECESSOR edge to some node key *)

  type tbl = (tbl_key, tbl_entry) Hashtbl.t

  let to_node_list (edges : Node.t edge list) : Node.t list =
    let module NodeSet = Set.Make (Node) in
    let node_set = List.fold_left (fun acc (from_, to_, _) ->
      acc
      |> NodeSet.add from_
      |> NodeSet.add to_
    ) NodeSet.empty edges
    in
    NodeSet.to_list node_set

  let create_tbl ~(src : Node.t) (edges : Node.t edge list) =
    let bindings =
      edges
      |> to_node_list
      |> List.map (fun node -> node, (None, None))
      |> List.to_seq
    in
    let tbl = Hashtbl.of_seq bindings in
    let () =
      Hashtbl.replace tbl src (Some 0, None)
    in
    tbl

  let set_distance (node : tbl_key) ~(min : int) ~(pred : Node.t edge) (tbl : tbl) : bool =
    Hashtbl.replace tbl node (Some min, Some pred);
    true

  let relax_edge (tbl : tbl) (was_updated : bool) ((from_, to_, cost) as edge : Node.t edge) : bool =
    match Hashtbl.find tbl from_, Hashtbl.find tbl to_ with
    | (Some du, _), (None, _) ->
      set_distance to_ tbl ~min:(du + cost) ~pred:edge
    | (Some du, _), (Some dv, _) when du + cost < dv ->
      set_distance to_ tbl ~min:(du + cost) ~pred:edge
    | _ -> was_updated

  let relax_edges (edges : Node.t edge list) (tbl : tbl) (i : int)
    : [ `Continue of tbl | `Stop of tbl ] =
    if i >= (Hashtbl.length tbl) - 1 then `Stop tbl
    else
      let is_dist_updated = List.fold_left (relax_edge tbl) false edges in
      if is_dist_updated then `Continue tbl
      else `Stop tbl

  let find_shortest_paths ~(src : Node.t) (edges : Node.t edge list) : tbl =
    let dist = create_tbl ~src edges in
    let num_nodes = Hashtbl.length dist in
    let num_nodes_range = List.init num_nodes Fun.id in
    let final_tbl = List_utils.fold_until
      (relax_edges edges)
      Fun.id
      dist
      num_nodes_range
    in
    final_tbl

  let find_distance (node : Node.t) (dist : tbl) : int =
    match fst @@ Hashtbl.find dist node with
    | None -> Int.max_int
    | Some v -> v

  let find_predecessor_edge (node : Node.t) (dist : tbl)
    : Node.t edge option =
    snd @@ Hashtbl.find dist node

  let find_predecessor (node : Node.t) (dist : tbl) : Node.t option =
    Option.map (fun (from_, _, _) -> from_) (find_predecessor_edge node dist)
    
  let find_relaxed_node_opt (edges : Node.t edge list) (dist : tbl) : Node.t option =
    List.find_map (fun ((_, to_, _) as edge) ->
      if relax_edge dist false edge then
        Some to_
      else None)
    edges

  let find_relaxed_node (edges : Node.t edge list) (tbl : tbl) : Node.t =
    match find_relaxed_node_opt edges tbl with
    | Some node -> node
    | None -> failwith "No relaxed node found"

  let find_cycle_node_opt (edges : Node.t edge list) (dist : tbl)
    : Node.t option =
    let num_nodes = Hashtbl.length dist in
    let relaxed_predecessor = find_relaxed_node_opt edges dist in
    let rec move_back node n =
      if n = 0 then node
      else
        match find_predecessor node dist with
        | None -> node
        | Some from_ -> move_back from_ (n - 1)
    in
    Option.map (fun entry -> move_back entry num_nodes) relaxed_predecessor
      
  let find_cycle_node (edges : Node.t edge list) (dist : tbl) : Node.t =
    match find_cycle_node_opt edges dist with
    | Some entry -> entry
    | None -> failwith "No negative cycle found"

  let collect_cycle (start : Node.t) (dist : tbl) : Node.t edge list =
    let num_nodes = Hashtbl.length dist in
    let rec loop curr n acc =
      if n = 0 then
        acc
      else
        match find_predecessor_edge curr dist with
        | None -> acc
        | Some ((from_, _, _) as pred_edge) ->
          let acc = pred_edge :: acc in
          if Node.compare from_ start = 0 then acc
          else loop from_ (n - 1) acc
    in
    loop start num_nodes []
end

let bellman_ford
  (type node)
  (module Node : Baby.OrderedType with type t = node)
 ~(src : node)
  (edges : node edge list)
  : [ `No_negative_cycle of (node * int) list
    | `Negative_cycle of node edge list
    ] =
  let open Make (Node) in
  let tbl = find_shortest_paths ~src edges in
  match find_cycle_node_opt edges tbl with
  | None -> `No_negative_cycle (
    tbl
    |> Hashtbl.to_seq_keys
    |> Seq.map (fun node -> node, find_distance node tbl)
    |> List.of_seq
  )
  | Some entry -> `Negative_cycle (collect_cycle entry tbl)
