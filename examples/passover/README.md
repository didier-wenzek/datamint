# PassOver

## Current count of visitors per room

* A stream of json messages received via HTTP
* The message are pushed into a Kafka topic
* The messages are decoded as position events
* Each message provides: a visitor id, a room id and a timestamp.
* The messages are pushed to a Kafka topic where there are partitioned by visitor.
* Ill formatted messages are log into a file.
* A first table maintains the current room of each visitor.
* A second table maintains the current count of visitor per room.
* The count of visitor per room is published over a web socket.

