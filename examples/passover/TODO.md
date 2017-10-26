* Add kafka topic as a way to log consumed messages.
* A message must be produced to the right partition.
* Log parse errors
* The most recent position event must be registered, no the last one. 
* Each instance of a runner must have its own set of databases
* Each instance of a runner must have its own set of state files
* Each instance of a runner must have its own set of error files
* Fix error: Kyoto.Error("opening the destination failed")
* Implement an event generator.
