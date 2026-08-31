type _ eff += Pause : unit eff

module Pause_effect = struct
  let yield () = Effect.perform Pause
end

type r =
  | Done
  | Cont of (unit, r) Effect.Deep.continuation
  | Spawn of work_item list
  | Cancel_peers of Utils.Pos.t

and work_item =
  { loc : Utils.Pos.t
  ; task : unit -> r
  }

let round_robin (fs : work_item list) : unit =
  let run_q = Queue.of_seq (List.to_seq fs) in
  let cancelled : (Utils.Pos.t, unit) Hashtbl.t = Hashtbl.create 16 in
  let is_cancelled loc = Hashtbl.mem cancelled loc in
  let enqueue item = Queue.push item run_q in
  let enqueue_cont loc k =
    enqueue { loc ; task = fun () -> Effect.Deep.continue k () }
  in
  let cancel s = Hashtbl.replace cancelled s () in
  let rec dequeue () =
    begin match Queue.take_opt run_q with
    | None -> ()
    | Some { loc ; task = _ } when is_cancelled loc -> dequeue ()
    | Some { loc ; task } ->
      let r =
        try task () with
        | effect Pause, k -> Cont k
      in
      let () =
        match r with
        | Done -> ()
        | Cont k -> enqueue_cont loc k
        | Spawn children -> List.iter enqueue children
        | Cancel_peers s -> cancel s
      in
      dequeue ()
    end
  in
  dequeue ()
