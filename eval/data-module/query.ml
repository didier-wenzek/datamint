module type S = sig
  type ('a,'b) relation
  type 'a collection
  type 'a result

  type 'a clause
  type 'a query
  type ('a,'b) var

  val var1 : unit -> ('a,'a) var
  val var2 : unit -> ('a,'a*'b) var * ('b,'a*'b) var
  val var3 : unit -> ('a,'a*'b*'c) var * ('b,'a*'b*'c) var * ('c,'a*'b*'c) var
  val var4 : unit -> ('a,'a*'b*'c*'d) var * ('b,'a*'b*'c*'d) var * ('c,'a*'b*'c*'d) var * ('d,'a*'b*'c*'d) var
  val var5 : unit ->
           ('a, 'a * ('b * ('c * ('d * ('e * 'f))))) var *
           ('b, 'a * ('b * ('c * ('d * ('e * 'f))))) var *
           ('c, 'a * ('b * ('c * ('d * ('e * 'f))))) var *
           ('d, 'a * ('b * ('c * ('d * ('e * 'f))))) var *
           ('e, 'a * ('b * ('c * ('d * ('e * 'f))))) var

  val __ : ('a,'b) var
  val value: 'a -> ('a,'b) var

  val (!!): ('a,'c) var -> ('a,'b) relation -> ('b,'c) var -> 'c clause

  type ('a,'b) selection
  val all: ('a,'a) selection
  val (!$): ('a,'b) var -> ('a,'b) selection
  val ($): ('a,'c) selection -> ('b,'c) var -> ('a*'b, 'c) selection

  val select: ('a,'b) selection -> 'b clause list -> 'a query
  val run: 'a query -> 'a result
end
