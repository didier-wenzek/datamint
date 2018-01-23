type gen_cap = Capability.some
(** The type [gen_cap] tags the capability for a collection to generate its elements. *)

(** Signature of modules aimed to interpret a query plan. *)
module type S = sig
  
  type ('a,'b,'ab_gen,'a_gen,'b_gen) relation
  (** A relation between ['a] and ['b] values.

      A relation of type [('a,'b,'ab_gen,'a_gen,'b_gen)] is tagged with the following capabilities:
      - ['ab_gen] the capability to generate all pairs
      - ['a_gen]  the capability to generate all right values associated to a left value
      - ['a_gen]  the capability to generate all left values associated to a right value. *)

  type ('a,'a_gen) collection
  (** A collection of ['a] values.

      A collection of type [('a,'a_gen) collection]
      is equivalent to a relation of type [(unit,'a,'a_gen,'a_gen,'a_gen) relation].
      
      A collection of type [('a,'a_gen) collection] is tagged with:
      - ['a_gen] the capability to generate all elements. *)

  type ('a,'b) reducer
  (** A reducer of type [('a,'b) reducer] *)

  type 'a records
  (** A stream of records.

      A query will be interpreted as a transformation of some ['a records] into some ['b records]. *)

  type ('a,'b) extractor = 'a -> 'b
  (** Extract a field of type ['b] from a record of type ['a]..

      Typically equals to the function type: ['a -> 'b]. *)

  type ('a,'b,'c) injector = 'a -> 'b -> 'c
  (** Inject a field value of type ['b] into a record of type ['a],
      making an extended record of type ['c]..

      Typically equals to the function type: ['a -> 'b -> 'c]. *)

  val record_source: unit records
  (** The source of a query pipeline. *)

  val map: ('a -> 'b) -> 'a records -> 'b records

  val generate: ('a,'b, gen_cap,'a_gen,'b_gen) relation -> ('c,'a,'d) injector ->  ('d,'b,'e) injector -> 'c records -> 'e records
  (** Generate all pairs of a relation.

      The relation must have the capability to generate its pairs.
      Use the two injectors to extend a source record with the left and right value of the pair. *)

  val gen_cap: ('a,'b, 'ab_gen,'a_gen,'b_gen) relation -> (('c,'a,'d) injector ->  ('d,'b,'e) injector -> 'c records -> 'e records, 'ab_gen) Capability.t

  val map_rel: ('a,'b, 'ab_gen,gen_cap,'b_gen) relation -> ('c,'a) extractor -> ('c,'b,'d) injector -> 'c records -> 'd records
  (** Map left values to their related right values.

      The relation must have the capability to generate right values given a left value.
      Use the extractor function to get the left value of an input record.
      Use the injector function to add a right value into a source record. *)

  val map_cap: ('a,'b, 'ab_gen,'a_gen,'b_gen) relation -> (('c,'a) extractor -> ('c,'b,'d) injector -> 'c records -> 'd records, 'a_gen) Capability.t

  val inv_rel: ('a,'b, 'ab_gen,'a_gen,gen_cap) relation -> ('c,'a,'d) injector -> ('c,'b) extractor -> 'c records -> 'd records
  (** Map right values to their related left values.

      The relation must have the capability to generate left values given a right value.
      Use the extractor function to get the right value of an input record.
      Use the injector function to add a left value into a source record. *)

  val inv_cap: ('a,'b, 'ab_gen,'a_gen,'b_gen) relation -> (('c,'a,'d) injector -> ('c,'b) extractor -> 'c records -> 'd records, 'b_gen) Capability.t

  val filter: ('a,'b, 'ab_gen,'a_gen,'b_gen) relation -> ('c,'a) extractor -> ('c,'b) extractor -> 'c records -> 'c records
  (** Filter pairs which are unrelated.

      Use the extractor functions to get the pair of an input record. *)

  val generate_member: ('a, gen_cap) collection -> ('b,'a,'c) injector -> 'b records -> 'c records
  (** Generates all elements of a collection.

      The collection must have the capability to generate its elements.
      Use the injector function to add an element into a source record. *)

  val filter_member: ('a,'a_gen) collection -> ('b,'a) extractor -> 'b records -> 'b records
  (** Filter items which are not element of the collection.

      Use the extractor function to get the candidate item of an input record. *)

  val reduce: ('a,'b) reducer -> ('c,'a) extractor -> 'c records -> ('b,gen_cap) collection
  (** Reduce a stream of records into a collection.

      Use the extractor function to get the field to be reduced. *)

  val group: ('a,'b) extractor -> ('a,'c) extractor -> 'a records -> ('b,'c,gen_cap,gen_cap,gen_cap) relation

  val group_reduce: ('c,'d) reducer -> ('a,'b) extractor -> ('a,'c) extractor -> 'a records -> ('b,'d,gen_cap,gen_cap,gen_cap) relation

  val rel_of_col: ('a,'a_gen) collection -> (unit,'a,'a_gen,'a_gen,gen_cap) relation
  (** [rel_of_col c] is the relation which associates the unit value with the collection [c]. *)

  val col_of_rel: (unit,'a,'a_gen,'a_gen,gen_cap) relation -> ('a,'a_gen) collection
  (** [col_of_rel r] is the collection associated by the relation [r] to the unit value. *)
end
