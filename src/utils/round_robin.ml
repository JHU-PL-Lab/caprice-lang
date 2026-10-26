
type _ eff += Pause : unit eff
exception Cancel

module Pause_effect = struct
  let yield () = Effect.perform Pause
end

module Make (Id : Types.CMP) (Res : Types.MONOID) = struct
  type r =
    | Done of Res.t
    | Cont of (unit, r) Effect.Deep.continuation
    | Spawn of work_item list
    | Cancel_peers of Id.t

  (* Only one of task and cancel may be called. *)
  and work_item =
    { loc : Id.t
    ; task : unit -> r
    ; cancel : unit -> unit
    }

  let filter_queue f q =
    for _ = 1 to Queue.length q do
      let x = Queue.pop q in
      if f x then Queue.push x q
    done

  let round_robin (fs : work_item list) : Res.t =
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
    let acc = ref Res.neutral in
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
          | Done res -> acc := Res.combine res !acc
          | Cont k -> enqueue_cont loc k
          | Spawn children -> List.iter enqueue children
          | Cancel_peers id ->
            filter_queue (fun { loc ; task = _ ; cancel } ->
              if Id.compare id loc = 0 then
                (cancel (); false)
              else
                true
            ) run_q
        in
        dequeue ()
    in
    try dequeue (); !acc with
    | exn ->
      filter_queue (fun { cancel ; _ } -> cancel (); false) run_q;
      raise exn
end
