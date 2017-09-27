module type S = sig
  type ('a,'b) relation
  type 'a collection

  val eq: ('a,'a) relation
  val ne: ('a,'a) relation
  val lt: ('a,'a) relation
  val gt: ('a,'a) relation
  val le: ('a,'a) relation
  val ge: ('a,'a) relation

  val (<=>): ('a,'b) relation -> ('b,'c) relation -> ('a,'c) relation
  val inverse: ('a,'b) relation -> ('b,'a) relation
  val rel_of_col: 'a collection -> (unit,'a) relation
  val col_of_rel: (unit,'a) relation -> 'a collection

  type 'a value
  type ('a,'b) extractor
  type ('a,'b,'c) injector

  val unit_value: unit value

  val filter: ('a,'b) relation -> (('c,'a) extractor -> ('c,'b) extractor -> 'c value -> 'c value) option
  val map: ('a,'b) relation -> (('c,'a) extractor -> ('c,'b,'d) injector -> 'c value -> 'd value) option
  val inv_map: ('a,'b) relation -> (('c,'a,'d) injector -> ('c,'b) extractor -> 'c value -> 'd value) option
  val generate: ('a,'b) relation -> (('c,'a,'d) injector ->  ('d,'b,'e) injector -> 'c value -> 'e value) option

  val generate_members: ('a) collection -> (('b,'a,'c) injector -> 'b value -> 'c value) option
  val filter_members: ('a) collection -> (('b,'a) extractor -> 'b value -> 'b value) option
end
