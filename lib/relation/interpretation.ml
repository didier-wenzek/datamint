
type gen_cap = Capability.some


(** Signature of modules aimed to interpret a query plan.

*)
module type S = sig
  
  (** A relation between ['a] and ['b] values.

    A given relation implementation may have specific capabilities:
    - to generate all pairs
    - to generate all right values associated to a left value
    - to generate all left values associated to a right value.*)
  type ('a,'b,'ab_gen,'a_gen,'b_gen) relation

  type ('a,'a_gen) collection

  type ('a,'b) reducer

  (* A stream of records. *)
  type 'a records

  (* Extract a field of a record.

     Typically equals to the function type: ['a -> 'b]. *)
  type ('a,'b) extractor

  (* Inject a field value into a record, making an extended record.

     Typically equals to the function type: ['a -> 'b -> 'c]. *)
  type ('a,'b,'c) injector

  (** The stream to be used as the source of a query pipeline. *)
  val record_source: unit records

  (** Generates all pairs of a relation.

      The relation must have the capability to generate its pairs.
      Use the injectors to extend a source record with the left and right value of the pair. *)
  val generate: ('a,'b, gen_cap,'a_gen,'b_gen) relation -> ('c,'a,'d) injector ->  ('d,'b,'e) injector -> 'c records -> 'e records

  (** Map left values to their related right values.

      The relation must have the capability to generate right values given a left value.
      Use the extractor function to get the left value of an input record.
      Use the injector function to add a right value into a source record. *)
  val map: ('a,'b, 'ab_gen,'a_gen,gen_cap) relation -> ('c,'a) extractor -> ('c,'b,'d) injector -> 'c records -> 'd records

  (** Map right values to their related left values.

      The relation must have the capability to generate left values given a right value.
      Use the extractor function to get the right value of an input record.
      Use the injector function to add a left value into a source record. *)
  val inv_map: ('a,'b, 'ab_gen,gen_cap,'b_gen) relation -> ('c,'a,'d) injector -> ('c,'b) extractor -> 'c records -> 'd records

  (** Filter pairs which are unrelated.

      Use the extractor functions to get the pair of an input record. *)
  val filter: ('a,'b, 'ab_gen,'a_gen,'b_gen) relation -> ('c,'a) extractor -> ('c,'b) extractor -> 'c records -> 'c records

  (** Generates all elements of a collection.

      The collection must have the capability to generate its elements.
      Use the injector function to add an element into a source record. *)
  val generate_member: ('a, gen_cap) collection -> ('b,'a,'c) injector -> 'b records -> 'c records

  (** Filter items which are not element of the collection.

      Use the extractor function to get the candidate item of an input record. *)
  val filter_member: ('a,'a_gen) collection -> ('b,'a) extractor -> 'b records -> 'b records

  val reduce: ('a,'b) extractor -> ('b,'c) reducer -> 'a records -> ('c,gen_cap) collection
  val group: ('a,'b) extractor -> ('a,'c) extractor -> 'a records -> ('b,'c,gen_cap,gen_cap,gen_cap) relation
  val group_reduce: ('a,'b) extractor -> ('a,'c) extractor -> ('c,'d) reducer -> 'a records -> ('b,'d,gen_cap,gen_cap,gen_cap) relation

  val rel_of_col: ('a,'a_gen) collection -> (unit,'a,'a_gen,gen_cap,'a_gen) relation
  val col_of_rel: (unit,'a,'a_gen,gen_cap,'a_gen) relation -> ('a,'a_gen) collection
end
