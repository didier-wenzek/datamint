module type S = sig
  include Interpretation.S

  type 'a clause
  type 'a query
  type ('a,'b) var

  val var1 : unit ->
           ('a, 'a * unit) var

  val var2 : unit ->
           ('a, 'a * ('b * unit)) var *
           ('b, 'a * ('b * unit)) var

  val var3 : unit ->
           ('a, 'a * ('b * ('c * unit))) var *
           ('b, 'a * ('b * ('c * unit))) var *
           ('c, 'a * ('b * ('c * unit))) var

  val var4 : unit ->
           ('a, 'a * ('b * ('c * ('d * unit)))) var *
           ('b, 'a * ('b * ('c * ('d * unit)))) var *
           ('c, 'a * ('b * ('c * ('d * unit)))) var *
           ('d, 'a * ('b * ('c * ('d * unit)))) var

  val var5 : unit ->
           ('a, 'a * ('b * ('c * ('d * ('e * unit))))) var *
           ('b, 'a * ('b * ('c * ('d * ('e * unit))))) var *
           ('c, 'a * ('b * ('c * ('d * ('e * unit))))) var *
           ('d, 'a * ('b * ('c * ('d * ('e * unit))))) var *
           ('e, 'a * ('b * ('c * ('d * ('e * unit))))) var

  val __ : ('a,'b) var
  val value: 'a -> ('a,'b) var

  val (!!): ('a,'c) var -> ('a,'b, 'ab_gen, 'a_gen, 'b_gen) relation -> ('b,'c) var -> 'c clause

  type ('a,'b) selection
  val all: ('a,'a) selection
  val (!$): ('a,'b) var -> ('a,'b) selection
  val ($): ('a,'c) selection -> ('b,'c) var -> ('a*'b, 'c) selection

  val select: ('a,'b) selection -> 'b clause list -> 'a query
  val run: 'a query -> 'a records
end
