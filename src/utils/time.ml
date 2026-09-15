
let time f x =
  let c = Mtime_clock.counter () in
  let res = f x in
  Mtime_clock.count c, res

let of_float_sec float_sec =
  let s_per_ns = Mtime.Span.to_float_ns Mtime.Span.s /. Mtime.Span.to_float_ns Mtime.Span.ns in
  Mtime.Span.of_float_ns (float_sec *. s_per_ns)

let convert_span span ~to_ =
  let ns_per_t = Mtime.Span.to_float_ns Mtime.Span.ns /. Mtime.Span.to_float_ns to_ in
  Mtime.Span.to_float_ns span *. ns_per_t

let span_to_ms = convert_span ~to_:Mtime.Span.ms

let argv_span_conv =
  Cmdliner.Arg.Conv.make ()
    ~docv:"SPAN_SEC"
    ~parser:(fun s ->
        let span_opt = Option.bind (Float.of_string_opt s) of_float_sec in
        match span_opt with
        | Some span -> Ok span
        | None -> Error "Invalid time span as float seconds"
      )
    ~pp:Mtime.Span.pp

type _ eff += Check_timeout : unit eff

let yield_to_timer () = Effect.perform Check_timeout

let with_timeout span f a =
  let timer = Mtime_clock.counter () in
  try
    Ok (f a)
  with
  | effect Check_timeout, k ->
    let t = Mtime_clock.count timer in
    if Mtime.Span.is_longer t ~than:span then
      let exception Timeout in
      try Effect.Deep.discontinue k Timeout with
      | Timeout -> Error t
    else
      Effect.Deep.continue k ()
