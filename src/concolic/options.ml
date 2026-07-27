
type splay =
  | Splay_only  (* type splay only *)
  | Never_splay (* do not type splay *)
  | Fallback    (* type splay, then fall back to no type splay if error *)

type t =
  { max_tree_depth : int
  ; max_step       : Grammar.Step.t
  ; global_timeout : Mtime.Span.t
  ; splay          : splay
  ; do_wrap        : bool
  ; do_fork        : bool
  ; is_random      : bool
  }

let default : t =
  { max_tree_depth = 30
  ; max_step       = Step 100_000
  ; global_timeout = Mtime.Span.(10 * s)
  ; splay          = Fallback
  ; do_wrap        = true
  ; do_fork        = true
  ; is_random      = false
  }

let of_argv =
  let open Cmdliner.Term.Syntax in
  let open Cmdliner.Arg in
  let+ max_tree_depth =
    value & opt int default.max_tree_depth
    & info ["d"; "depth"] ~docv:"DEPTH" ~doc:"Maximum tree depth"
  and+ max_step =
    value & opt Grammar.Step.argv_step_conv default.max_step
    & info ["m"; "max-step"] ~docv:"MAX_STEP" ~doc:"Maximum step count per evaluation"
  and+ global_timeout =
    value & opt Utils.Time.argv_span_conv default.global_timeout
    & info ["t"; "timeout"] ~docv:"TIMEOUT" ~doc:"Global timeout seconds"
  and+ splay =
    value & opt (enum (["only", Splay_only; "never", Never_splay; "fallback", Fallback])) default.splay
    & info ["s"; "splay"] ~doc:"Type splay: only, never, or fallback. Default is fallback."
  and+ do_wrap =
    value & opt (enum (["yes", true; "no", false])) default.do_wrap
    & info ["w"; "wrap"] ~doc:"Wrap flag: yes or no. Default is yes."
  and+ do_fork =
    value & opt (enum (["yes", true; "no", false])) default.do_fork
    & info ["f"; "fork"] ~doc:"Fork flag: yes or no. Default is yes."
  and+ is_random =
    value & flag & info ["r"; "random"] ~doc:"Randomize"
  in
  { max_tree_depth ; max_step ; global_timeout ; splay ; do_wrap ; do_fork ; is_random }

