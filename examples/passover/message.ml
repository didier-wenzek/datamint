open Series.Util

type position_event = {
  visitor: string;
  room: string;
  timestamp: Int64.t;
}

let position_message =
  let open Generics.Repr in
  record (
    field "visitor" string
  & field "room" string
  & field "timestamp" int64
  )

let position_event_of_message (visitor, (room, timestamp)) =
  { visitor; room; timestamp }

let position_message_of_event { visitor; room; timestamp } =
  (visitor, (room, timestamp))

let visitor_of_event e = e.visitor
let room_of_event e = e.room
let timestamp_of_event e = e.timestamp

let json = Generics.Json_support.format

let position_of_json =
  Generics.Serialization.format_decoder json position_message
  >> Result.map position_event_of_message

let json_of_position =
  position_message_of_event
  >> Generics.Serialization.format_encoder json position_message
  >> Result.on_error (fun e -> assert false)

type 'a update =
  | Insert of 'a
  | Remove of 'a

let insert x = Insert x
let remove x = Remove x

let move_message =
  let open Generics.Repr in
  record (
    field "visitor" string
  & field "room" string
  )

let move_of_json =
  Generics.Serialization.format_decoder json move_message

let json_of_move =
  Generics.Serialization.format_encoder json move_message
  >> Result.on_error (fun e -> assert false)

let string_of_move = function
  | Insert v -> Format.sprintf "+ %s" (json_of_move v)
  | Remove v -> Format.sprintf "- %s" (json_of_move v)

let move_of_string s =
  let c = String.sub s 0 1 in
  let v = move_of_json (String.sub s 1 ((String.length s) - 1)) in
  if c = "-"
  then Result.map remove v
  else Result.map insert v
  
  

