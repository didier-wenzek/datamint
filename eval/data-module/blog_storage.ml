module Make : Blog_schema.S = struct

  include Query_refimpl.Make( Schema_refimpl.Make(Dataset_refimpl.Bag))

  type author 
  type post
  type comment
  type uuid = string
  type tag = string
  type date = int


end
