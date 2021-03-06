module Bag = Dataset_refimpl.Bag
type 'a bag = 'a Bag.t
type ('a,'b) index = ('a,'b) Bag.mapping

type uuid = string
type tag = string
type date = int

module DB_repr = struct
  module Author = struct
    type row =  { uuid: uuid; name: string }
  end

  module Post = struct
    type row = { uuid: uuid; author_uuid: uuid; date: date; title: string; tags: tag bag; content: string;}
  end

  module Comment = struct
    type row = { uuid: uuid; author_uuid: uuid; date: date; post_uuid: uuid; content: string;}
  end
end

module type DB = sig
  include module type of DB_repr

  val authors: (uuid, Author.row) index
  val comments: (uuid, Comment.row) index
  val posts: (uuid, Post.row) index
end

module EmptyDB : DB = struct
  include DB_repr

  let authors = Bag.empty_mapping ()
  let posts = Bag.empty_mapping ()
  let comments = Bag.empty_mapping ()
end

module Open(DB: DB) : Blog_schema.S
  with type 'a value = 'a Bag.t
= struct
  include Schema_refimpl.Make(Bag)

  type author = DB.Author.row
  type post = DB.Post.row
  type comment = DB.Comment.row

  let authors = DB.authors |> Bag.values |> collection_of_dataset
  let posts = DB.posts |> Bag.values |> collection_of_dataset
  let comments = DB.comments |> Bag.values |> collection_of_dataset

  let author_of_uuid = relation_of_mapping_with_inv DB.authors (fun a -> a.uuid)
  let post_of_uuid = relation_of_mapping_with_inv DB.posts (fun p -> p.uuid)
  let comment_of_uuid = relation_of_mapping_with_inv DB.comments (fun c -> c.uuid)

  module Author = struct
    open DB.Author
    let uuid = inverse author_of_uuid
    let name = relation_of_function (fun a -> a.name)
  end

  module Post = struct
    open DB.Post
    let uuid = inverse post_of_uuid
    let author_uuid = relation_of_function (fun p -> p.author_uuid)
    let date = relation_of_function (fun p -> p.date)
    let title = relation_of_function (fun p -> p.title)
    let tags = relation_of_plural_function (fun p -> p.tags)
    let content = relation_of_function (fun p -> p.content)

    let author = author_uuid <=> author_of_uuid
  end

  module Comment = struct
    open DB.Comment
    let uuid = inverse comment_of_uuid
    let author_uuid = relation_of_function (fun c -> c.author_uuid)
    let date = relation_of_function (fun c -> c.date)
    let post_uuid = relation_of_function (fun c -> c.post_uuid)
    let content = relation_of_function (fun c -> c.content)

    let author = author_uuid <=> author_of_uuid
    let post = post_uuid <=> post_of_uuid
  end
end
