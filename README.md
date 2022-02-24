The primary goal of Data-Mint is to ease data processing with:

* a set of query tools and languages
* a separation of query definitions from both data storage and query execution scheduling.

## Build

```shell
$ opam install dune

# lwt is used with libev
$ sudo apt-get install libev-dev
$ opam install conf-libev
$ opam install lwt

$ opam install ppx_deriving yojson lambda-term

# Some examples use kyoto
$ sudo apt-get install libkyotocabinet-dev
$ opam install kyotocabinet

# Some examples use kafka
$ sudo apt-get install librdkafka-dev
$ opam install kafka_lwt

$ make
```
