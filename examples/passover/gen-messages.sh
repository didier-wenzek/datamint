for ROOM in $(seq 100000)
do
  echo '{ "visitor": "V001", "room": ' "\"R$ROOM\"" ', "timestamp":' $(date +%s) '}'
done | sendto_kafka_topic passover.positions.messages
