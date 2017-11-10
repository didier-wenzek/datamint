module type Monoid = sig
  type 'a t
  type 'a elt

  val empty: 'a t
  val single: 'a elt -> 'a t
  val merge: 'a t -> 'a t -> 'a t
end

module Sum : Monoid
  with type 'a elt = int
= struct
  type 'a t = int
  type 'a elt = int

  let empty = 0
  let single x = x
  let merge = (+)
end

module Count : Monoid
  with type 'a elt = 'a
= struct
  type 'a t = int
  type 'a elt = 'a

  let empty = 0
  let single x = 1
  let merge = (+)
end

module Col : Monoid
  with type 'a elt = 'a
= struct
  type 'a t = Empty | Single of 'a | Merge of 'a t * 'a t
  type 'a elt = 'a

  let empty = Empty
  let single x = Single x
  let merge a b = Merge (a,b)
end

module Group(K: Map.OrderedType)(M: Monoid) : Monoid
  with type 'a elt = K.t * 'a M.elt
= struct
  module G = Map.Make(K) 

  type 'a t = 'a M.t G.t
  type 'a elt = K.t * 'a M.elt

  let empty = G.empty
  let single (k,v) = G.singleton k (M.single v)
  let merge kvs = G.merge (fun k a b -> match a,b with
    | Some a, Some b -> Some (M.merge a b)
    | None, s | s, None -> s
  ) kvs
end

module type S = sig
  type ('a,'b) relation
  type 'a collection
  type ('a,'b) reducer

  val eq: ('a,'a) relation
  val ne: ('a,'a) relation
  val lt: ('a,'a) relation
  val gt: ('a,'a) relation
  val le: ('a,'a) relation
  val ge: ('a,'a) relation

  val (<=>): ('a,'b) relation -> ('b,'c) relation -> ('a,'c) relation
  val inverse: ('a,'b) relation -> ('b,'a) relation

  module Reduce (M: Monoid) : sig
    type 'a value = 'a M.t

    val select: 'a M.elt -> 'a value

    val generate: ('a,'b) relation -> ('a -> 'b -> 'c value) -> 'c value
    val map:      ('a,'b) relation -> 'a -> ('b -> 'c value) -> 'c value
    val inv_map:  ('a,'b) relation -> 'b -> ('a -> 'c value) -> 'c value
    val filter:   ('a,'b) relation -> 'a -> 'b -> 'c value -> 'c value

    val generate_members: ('a) collection -> ('a -> 'b value) -> 'b value
    val filter_members:   ('a) collection -> 'a -> 'b value -> 'b value
  end
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

let (@|) f x = f x

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

  module Collect = Reduce(Col)

  let posts_of_author name =
    let open Collect in
    inv_map Author.name name   @| fun author ->
    inv_map Post.author author @| fun post ->
    map Post.uuid post         @| fun uuid ->
    map Post.title post        @| fun title ->
    map Post.date post         @| fun date ->
    select (uuid,title,date)

  let posts_of_tag tag =
    let open Collect in
    inv_map Post.tags tag                  @| fun post ->
    map (Post.author <=> Author.name) post @| fun author_name ->
    map Post.uuid post                     @| fun uuid ->
    map Post.title post                    @| fun title ->
    map Post.date post                     @| fun date ->
    select (uuid,author_name,title,date)

  let authors_commenting_their_posts =
    let open Collect in
    generate (Author.posts <=> Post.comments <=> Comment.author) @| fun post_author comment_author ->
    filter eq post_author comment_author                         @|
    map Author.name post_author                                  @| fun author_name ->
    select author_name

  module ReduceCount = Reduce(Count)

  let count_of_posts =
    let open ReduceCount in
    generate_members posts @| fun post ->
    select post

  module GroupCount = Group(String)(Count)
  module ReduceGroupCount = Reduce(GroupCount)

  let count_of_posts_per_author =
    let open ReduceGroupCount in
    generate Author.posts  @| fun author post ->
    map Author.uuid author @| fun uuid ->
    select (uuid, post)

end
