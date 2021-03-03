open Lwt.Infix

type t = unit Lwt.t

let running_processes : unit Lwt.t list ref = ref []

let wait_processes () = Lwt.join !running_processes

let register_process p =
  running_processes := p :: !running_processes;
  p

let interrupted = ref false

let signals = [ 2; 15 ]

let () =
  let stop signal_handler _signal =
    interrupted := true;
    Lwt_unix.disable_signal_handler signal_handler
  in 
  List.iter (fun signal -> Lwt_unix.on_signal_full signal stop |> ignore) signals

let is_interrupted () = !interrupted

let loop_until_interrupted step =
  let rec loop () =
    step ()
    >>= fun () ->
    if !interrupted then Lwt.return_unit else loop ()
  in
  loop ()

type 'a step = Done of 'a | Continue of 'a | Interrupted of 'a

let step_value = function
  | Done x | Continue x | Interrupted x -> x

let loop_until_done_or_interrupted step =
  let rec loop s =
    step s >>= function
    | Continue s ->
      if !interrupted
      then Lwt.return (Interrupted s)
      else loop s
    | s -> Lwt.return s
  in
  loop

let loop_until_done step =
  let rec loop s =
    step s >>= function
    | Continue s when not !interrupted -> loop s
    | _ -> Lwt.return_unit
  in
  loop
