type position_event = {
  visitor: string;
  room: string;
  timestamp: Int64.t;
}

val position_of_json: string -> (position_event, string) result

val json_of_position: position_event -> (string, string) result
