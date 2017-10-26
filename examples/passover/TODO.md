* Add kafka topic as a way to log consumed messages.
* A message must be produced to the right partition.
* Log parse errors
* The most recent position event must be registered, no the last one. 
* Fix error: Kyoto.Error("opening the destination failed")
* How can we ensure that exactly one proccess run for each partition ?
* Implement an event generator.
