module type S = sig
  include Query.S

  type author
  type post
  type comment
  type tag = string
  type date = int

  val authors: author collection
  val posts: post collection
  val comments: comment collection

  module Author : sig
    val name: (author,string) relation
  end

  module Post : sig
    val author: (post,author) relation
    val date: (post,date) relation
    val title: (post,string) relation
    val tags: (post,tag) relation
    val content: (post,string) relation
  end

  module Comment : sig
    val author: (comment,author) relation
    val date: (comment,date) relation
    val post: (comment,post) relation
    val content: (comment,string) relation
  end
end