type _ eff += Pause : unit eff
exception Cancel

module Pause_effect = struct
  let yield () = Effect.perform Pause
end

type r =
  | Done
  | Cont of (unit, r) Effect.Deep.continuation
  | Spawn of work_item list
  | Cancel_peers of Utils.Pos.t

(* Only one of task and cancel may be called. *)
and work_item =
  { loc : Utils.Pos.t
  ; task : unit -> r
  ; cancel : unit -> unit
  }

let filter_queue f q =
  for _ = 1 to Queue.length q do
    let x = Queue.pop q in
    if f x then Queue.push x q
  done

let round_robin (fs : work_item list) : unit =
  let run_q = Queue.of_seq (List.to_seq fs) in
  let enqueue item = Queue.push item run_q in
  let enqueue_cont loc k =
    let task () =
      Effect.Deep.continue k ()
    and cancel () =
      match Effect.Deep.discontinue k Cancel with
      | exception Cancel -> ()
      | _ -> ()
    in
    enqueue { loc ; task ; cancel }
  in
  let rec dequeue () =
    match Queue.take_opt run_q with
    | None -> ()
    | Some { loc ; task ; cancel = _ } ->
      let r =
        try task () with
        | effect Pause, k -> Cont k
      in
      let () =
        match r with
        | Done -> ()
        | Cont k -> enqueue_cont loc k
        | Spawn children -> List.iter enqueue children
        | Cancel_peers pos ->
          filter_queue (fun { loc ; task = _ ; cancel } ->
            if pos.pos_cnum = loc.pos_cnum then
              (cancel (); false)
            else
              true
          ) run_q
      in
      dequeue ()
  in
  dequeue ()
