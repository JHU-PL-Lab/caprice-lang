(* Runs difference logic examples for the report *)

[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

open Utils

let bellman_ford = Bellman_ford.bellman_ford (module String)
module BellmanFord = Bellman_ford.Make (String)

let pp_markdown_distance_table ~src distances =
  print_endline "| Node | Distance |";
  print_endline "|------|----------|";

  List.iter
    (fun (node, distance) ->
      if node = src then ()
      else
        Printf.printf
          "| $%s$  |   $%s$    |\n"
          (if node = "0*" then "0^*" else node)
          (if distance = Int.max_int then "∞" else Int.to_string distance))
    distances
;;
let pp_mermaid_lr ?(id = "mermaid-graph") edges =
  let node_id name =
    "n" ^ String.map (function
      | '*' -> '_'
      | '-' -> '_'
      | c -> c)
      name
  in

  let pp_edge (from_, to_, cost) =
    Printf.sprintf
      "  %s[\"%s\"] -->|\"%d\"| %s[\"%s\"]"
      (node_id from_)
      from_
      cost
      (node_id to_)
      to_
  in

  let body =
    edges
    |> List.map pp_edge
    |> String.concat "\n"
  in

  Printf.sprintf
    "```{.mermaid #%s}\ngraph LR\n%s\n```"
    id
    body

let print_mermaid_lr ~id edges =
  print_endline (pp_mermaid_lr ~id edges)

let pp_mermaid_cycle ~label cycle_edges =
  Printf.printf "```{.mermaid #%s}\n" label;
  print_endline "graph LR";

  List.iter
    (fun (from_, to_, weight) ->
      Printf.printf
        "  n%s[\"%s\"] -->|\"%d\"| n%s[\"%s\"]\n"
        from_
        from_
        weight
        to_
        to_)
    cycle_edges;

  print_endline "```"
;;

let pp_result ~src ~label result =
  match result with
  | `No_negative_cycle distances ->
    print_endline "```\nNo negative cycle found.\n```";
    print_endline "";
    pp_markdown_distance_table ~src distances

  | `Negative_cycle cycle_edges ->
    print_endline "```\nNegative cycle found!\n```";
    print_endline "";
    pp_mermaid_cycle ~label cycle_edges
;;

let print_bellman_ford ~label ~src edges =
  Printf.printf "(%s)\n" label;
  pp_result ~src ~label (bellman_ford ~src edges);
;;

let print_find_relaxed_node_opt edges dist =
  match BellmanFord.find_relaxed_node_opt edges dist with
  | None -> Printf.printf "No negative cycle!"
  | Some relnode -> Printf.printf "Relaxed: %s\n" relnode

let pp_edge (from_, to_, weight) =
  let from_ = if from_ = "0*" then "0^*" else from_ in
  let to_ = if to_ = "0*" then "0^*" else to_ in
  Printf.sprintf "%s -> %s (%d)" from_ to_ weight
  
let pp_edge_opt edge =
  match edge with
  | None -> "None"
  | Some e -> pp_edge e

let pp_result ~src result =
  match result with
  | `No_negative_cycle distances ->
    print_endline "No negative cycle found.";
    List.iter (fun (node, distance) ->
      if node = src then ()
      else
        Printf.printf "dist(%c) = %s\n"
          node
          (if distance = Int.max_int then "∞" else Int.to_string distance)
    ) distances

  | `Negative_cycle cycle_edges ->
    print_endline "Negative cycle found:";
    List.iter (fun edge ->
      Printf.printf "- %s\n" (pp_edge edge))
      cycle_edges

let print_min_distance_to key dist =
  Printf.printf "Minimum distance to \"%s\" = %d\n" key (BellmanFord.find_distance key dist)

let print_predecessor_edge_of key dist =
  let predecessor_edge_of_a = BellmanFord.find_predecessor_edge key dist in
  Printf.printf "Predecessor edge of \"%s\" is = %s\n" key (pp_edge_opt predecessor_edge_of_a)
   

let () =
  let simple_no_neg =
      [ ("a", "0*", 3) ; ("0*", "a", -1)
      ; ("r", "0*", 9) ; ("r", "a", 5)
      ] in
  print_mermaid_lr ~id:"simple-no-neg-mermaid" simple_no_neg;
  print_bellman_ford ~label:"simple-no-neg" ~src:"r" simple_no_neg;
  (* 1. print-simple-no-neg *)

  print_newline ();

  let simple_neg =
    [ ("a", "0*", 3) ; ("0*", "a", -4)
    ; ("r", "0*", 9) ; ("r", "a", 5)
    ] in
  print_mermaid_lr ~id:"simple-neg-mermaid" simple_no_neg;
  print_bellman_ford ~label:"simple-neg-bf" ~src:"r" simple_neg;
  (* 2. print-simple-neg *)

  let dist_simple_no_neg = BellmanFord.find_shortest_paths ~src:"r" simple_no_neg in

  print_min_distance_to "a" dist_simple_no_neg;
  print_predecessor_edge_of "a" dist_simple_no_neg;

  print_min_distance_to "0*" dist_simple_no_neg;
  print_predecessor_edge_of "0*" dist_simple_no_neg;

  let relnode_not_in_neg_cycle =
    [ ("c", "d", 0) ; ("s", "a", 0)
    ; ("a", "b", 1) ; ("b", "c", -4)
    ; ("c", "a", 1)
    ] in
  print_mermaid_lr ~id:"relnode-not-in-neg-cycle-mermaid" relnode_not_in_neg_cycle;

  let dist_relnode = BellmanFord.find_shortest_paths ~src:"s" relnode_not_in_neg_cycle in
  print_find_relaxed_node_opt relnode_not_in_neg_cycle dist_relnode;
  print_bellman_ford ~label:"relnode-not-in-neg-cycle-bf" ~src:"s" relnode_not_in_neg_cycle
