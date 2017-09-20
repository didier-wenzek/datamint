val is_interrupted: unit -> bool

type 'a step = Done of 'a | Continue of 'a | Interrupted of 'a
val step_value: 'a step -> 'a

val loop_until_done: ('a -> 'a step Lwt.t) -> 'a -> unit Lwt.t
val loop_until_interrupted: (unit -> unit Lwt.t) -> unit Lwt.t
val loop_until_done_or_interrupted: ('a -> 'a step Lwt.t) -> 'a -> 'a step Lwt.t
