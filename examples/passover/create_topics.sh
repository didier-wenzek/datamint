bin/kafka-topics.sh --create --partitions 3 --replication-factor 1 --zookeeper localhost --topic passover.positions.messages
bin/kafka-topics.sh --create --partitions 3 --replication-factor 1 --zookeeper localhost --topic passover.positions.events
