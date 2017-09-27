module type S = sig
  include Schema.S

  type ('a,'b) var

  val var1 : unit -> ('a,'a) var
  val var2 : unit -> ('a,'a*'b) var * ('b,'a*'b) var
  val var3 : unit -> ('a,'a*'b*'c) var * ('b,'a*'b*'c) var * ('c,'a*'b*'c) var
  val var4 : unit -> ('a,'a*'b*'c*'d) var * ('b,'a*'b*'c*'d) var * ('c,'a*'b*'c*'d) var * ('d,'a*'b*'c*'d) var
  val __ : ('a,'b) var
  val value: 'a -> ('a,'b) var

  type 'a clause
  type 'a query

  val (!!): ('a,'c) var -> ('a,'b) relation -> ('b,'c) var -> 'c clause
  val query: 'a clause list -> 'a query
end
