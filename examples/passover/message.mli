type position_event = {
  visitor: string;
  room: string;
  timestamp: Int64.t;
}

val visitor_of_event: position_event -> string
val room_of_event: position_event -> string
val timestamp_of_event: position_event -> Int64.t

val position_of_json: string -> (position_event, string) result
val json_of_position: position_event -> string

type 'a update =
  | Insert of 'a
  | Remove of 'a

val insert: 'a -> 'a update
val remove: 'a -> 'a update
val string_of_move: (string*string) update -> string
val move_of_string: string -> ((string*string) update, string) result
