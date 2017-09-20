(* Use a kyotocabinet file to store an incremental stream of (key-value) pairs *)
val store:
     encode_key:   ('a -> string)
  -> decode_key:   (string -> 'a)
  -> encode_value: ('b -> string)
  -> decode_value: (string -> 'b)
  -> file_path:    Series.Store.path
  ->               ('a, 'b) Series.Mapping.t Series.Store.store 
