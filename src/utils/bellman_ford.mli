(** ['node edge] is the 3-tuple (from, to, weight) where FROM and TO are some orderable NODE
    type and WEIGHT is an int value, since ceval only spits out int formulas *)
type 'node edge = 'node * 'node * int

(** [bellman_ford ~src edges] runs Bellman Ford to find the min distances
    from SRC node to all other nodes in EDGES and returns either...

    - [`No_negative_cycle dists] where each [dist] in [dists] is the (node, min_distance) pair
      whenever a graph without a negative cycle is passed in, {b or}

    - [`Negative_cycle cycle_edges] where each [cycle_edge] in [cycle_edges] is the connected
      edge list that make up the negative cycle in EDGES
*)
val bellman_ford :
  (module Baby.OrderedType with type t = 'node) ->
  src:'node ->
  'node edge list ->
  [ `No_negative_cycle of ('node * int) list
  | `Negative_cycle of 'node edge list
  ]

(** [Make Node] instantiates the Bellman Ford operations module. A NODE just needs a type [t]
    with a standard int returning [compare] function, so you can just about pass in any sortable type
    to represent the nodes in the Bellman Ford search you want to run... *)
module Make (Node : Baby.OrderedType) : sig
  (** Hashtbl key by [Node.t] indexes our current distance state to KEY node. *)
  type tbl_key = Node.t

  (** [(distance, predecessor_edge)] means for some key-ed node, our minimum DISTANCE path to that 
      node from our source ends with edge PREDECESSOR_EDGE. We use options on both to represent infinity
      and the null parent respectively.
  *)
  type tbl_entry = int option * Node.t edge option

  (** So we don't have to type out the full type each time... *)
  type tbl = (tbl_key, tbl_entry) Hashtbl.t

  (** [create_tbl ~src edges] returns the initial distance table state for a bellman ford run from
      SRC to all other nodes in EDGES *)
  val create_tbl : src:Node.t -> Node.t edge list -> tbl

  (** [set_distance node ~min ~pred dist] updates NODE's entry in DIST
      to new MIN with predecessor edge PRED and returns [true]. *)
  val set_distance : tbl_key -> min:int -> pred:Node.t edge -> tbl -> bool

  (** [relax_edge dist was_updated edge] updates DIST's table state for the [to_] node from EDGE
      [(from_, to_, cost)] when [dist[from_] + cost < dist[to_]] and returns [true]. Otherwise this
      returns [WAS_UPDATED || false].
  *)
  val relax_edge : tbl -> bool -> Node.t edge -> bool
  
  (** [relax_edges edges dist i] runs the I-th relaxation iteration over EDGES against distance
      table DIST and returns the next table state wrapped with either a [`Continue] or [`Stop]
      type flag indicating if the minimum distance path search has concluded or not

      It [`Stop]s for a graph with NUM_NODES nodes whenever...
      - We have iterated at least [NUM_NODES - 1] times ([I >= NUM_NODES - 1]) {b or}
      - We couldn't find a single edge to relax
        ([(relax_edge e1) || (relax_edge e2) || ... || (relax_edge e3)) = false]

      So the top-level iteration of NUM_NODES nodes will only [`Continue] when both
      - [I < NUM_NODES - 1] {b and}
      - At least one edge from EDGES was relaxed
        ([(relax_edge e1) || (relax_edge e2) || ... || (relax_edge e3)) = true]
  *)
  val relax_edges : Node.t edge list -> tbl -> int ->
    [ `Continue of tbl | `Stop of tbl ]

  (** [find_shortest_paths ~src edges] returns the minimum distance table
      of SRC to all other nodes in EDGES *)
  val find_shortest_paths : src:Node.t -> Node.t edge list -> tbl

  (** [find_distance node dist] returns the current shortest distance of NODE in DIST
      if it exists or [Int.int_max] otherwise *)
  val find_distance : Node.t -> tbl -> int

  (** [find_relaxed_node_opt edges dist] attempts to relax the distance in DIST for each edge 
      in EDGES and returns the `to_` node of the first edge that it relaxed, if it exists *)
  val find_relaxed_node_opt : Node.t edge list -> tbl -> Node.t option
  
  (** [find_relaxed_node edges dist] attempts to relax the distance in DIST for each edge 
      in EDGES and returns the `to_` node of the first edge that it relaxed, if it exists,
      otherwise it throws
  *)
  val find_relaxed_node : Node.t edge list -> tbl -> Node.t

  (** [find_cycle_node_opt edges dist] returns the first node from EDGES that in a part
      of the negative cycle recorded in DIST, if it exists *)
  val find_cycle_node_opt : Node.t edge list -> tbl -> Node.t option

  (** [find_cycle_node edges dist] returns the first node from EDGES that in a part
      of the negative cycle recorded in DIST, if it exists, otherwise this throws *)
  val find_cycle_node : Node.t edge list -> tbl -> Node.t

  (** [find_predecessor_edge node dist] returns the predecessor edge value of
      NODE in the DIST table, where an edge of [None] means there are currently
      no incoming nodes that are pointing to NODE in the current shortest path state
  *)
  val find_predecessor_edge : Node.t -> tbl -> Node.t edge option

  (** [find_predecessor node dist] returns the [to] node of the predecessor edge of NODE
      recorded in DIST table *)
  val find_predecessor : Node.t -> tbl -> Node.t option

  (** [collect_cycle node dist] returns the backtracked list of predecessor edges that compose
      the (typically) negative cycle recorded in DIST starting from cycle NODE *)
  val collect_cycle : Node.t -> tbl -> Node.t edge list
end
