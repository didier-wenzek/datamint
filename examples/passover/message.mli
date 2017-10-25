type position_event = {
  visitor: string;
  room: string;
  timestamp: Int64.t;
}

val visitor_of_event: position_event -> string
val room_of_event: position_event -> string
val timestamp_of_event: position_event -> Int64.t

val position_of_json: string -> (position_event, string) result

val json_of_position: position_event -> (string, string) result
