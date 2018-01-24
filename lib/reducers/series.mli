type 'a t

val fold: 'a t -> ('a,'b,'c) Action.t  -> 'b -> 'c

val fold_sync: 'a t -> ('a -> 'b -> 'b) -> 'b -> 'b

val fold_lwt: 'a t -> ('a -> 'b -> 'b Lwt.t) -> 'b -> 'b Lwt.t

val empty: 'a t

val single: 'a -> 'a t

val of_list: 'a list -> 'a t

val range: int -> int -> int t
