module type S = sig
  type ('a,'b) relation
  type 'a collection
  type 'a value
  type ('a,'b) reducer

  val eq: ('a,'a) relation
  val ne: ('a,'a) relation
  val lt: ('a,'a) relation
  val gt: ('a,'a) relation
  val le: ('a,'a) relation
  val ge: ('a,'a) relation

  val (<=>): ('a,'b) relation -> ('b,'c) relation -> ('a,'c) relation
  val inverse: ('a,'b) relation -> ('b,'a) relation

  val select: 'a -> 'a value
  val group: 'a -> 'b -> ('a*'b) value
  val reduce: ('a,'b) reducer -> 'a value -> 'b value
  val reduce_group: ('a,'b) reducer -> ('c*'a) value -> ('c*'b) value
  val count: ('a,int) reducer

  val generate: ('a,'b) relation -> ('a -> 'b -> 'c value) -> 'c value          (* FIXME: there are missing information           *)
  val map:      ('a,'b) relation -> 'a -> ('b -> 'c value) -> 'c value          (*        - what to do when the result is empty ? *)
  val inv_map:  ('a,'b) relation -> 'b -> ('a -> 'c value) -> 'c value          (*        - how to combine two results ?          *) 
  val filter:   ('a,'b) relation -> 'a -> 'b -> (unit-> 'c value) -> 'c value   (* Can we use implicit module ?                   *)

  val generate_members: ('a) collection -> ('a -> 'b value) -> 'b value
  val filter_members:   ('a) collection -> 'a -> (unit -> 'b value) -> 'b value
end

type uuid = string
type tag = string
type date = int

module type BS = sig
  include S

  type author
  type post
  type comment

  val authors: author collection
  val posts: post collection
  val comments: comment collection

  module Author : sig
    val uuid: (author,uuid) relation
    val name: (author,string) relation
  end

  module Post : sig
    val uuid: (post,uuid) relation
    val author: (post,author) relation
    val date: (post,date) relation
    val title: (post,string) relation
    val tags: (post,tag) relation
    val content: (post,string) relation
  end

  module Comment : sig
    val uuid: (comment,uuid) relation
    val author: (comment,author) relation
    val date: (comment,date) relation
    val post: (comment,post) relation
    val content: (comment,string) relation
  end
end

let ($$) f x = f x

module Comp(Schema: BS) = struct
  open Schema

  module Author = struct
    include Schema.Author
    let posts = inverse Post.author
    let comments = inverse Comment.author
  end

  module Post = struct
    include Schema.Post
    let comments = inverse Comment.post
  end

  let posts_of_author name =
    inv_map Author.name name $$ fun author ->
    inv_map Post.author author $$ fun post ->
    map Post.uuid post $$ fun uuid ->
    map Post.title post $$ fun title ->
    map Post.date post $$ fun date ->
    select (uuid,title,date)

  let posts_of_tag tag =
    inv_map Post.tags tag $$ fun post ->
    map (Post.author <=> Author.name) post $$ fun author_name ->
    map Post.uuid post $$ fun uuid ->
    map Post.title post $$ fun title ->
    map Post.date post $$ fun date ->
    select (uuid,author_name,title,date)

  let authors_commenting_their_posts =
    generate (Author.posts <=> Post.comments <=> Comment.author) $$ fun post_author comment_author ->
    filter eq post_author comment_author $$ fun () ->
    map Author.name post_author $$ fun author_name ->
    select author_name

  let count_of_posts =
    generate_members posts $$ fun post ->
    select post
    |> reduce count

  let count_of_posts_per_author =
    generate Author.posts $$ fun author post ->
    group author post
    |> reduce_group count

end
