module type S = sig
  include Monoid.S

  val remove: 'a elt -> 'a t -> 'a t
  val inverse: 'a t -> 'a t
end

module Extend(G: S) = struct
  include Monoid.Extend(G)
end
