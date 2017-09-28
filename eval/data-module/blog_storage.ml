module Make : Blog_schema.S = struct

  module Bag = Dataset_refimpl.Bag
  include Schema_refimpl.Make(Bag)
  type 'a bag = 'a Bag.t

  type uuid = string
  type tag = string
  type date = int

  module Author = struct
    type t = { uuid: uuid; name: string }
    let uuid = relation_of_function (fun a -> a.uuid)
    let name = relation_of_function (fun a -> a.name)
    let table = collection_of_list ([]: t list)
  end

  module Post = struct
    type t = { uuid: uuid; author_uuid: uuid; date: date; title: string; tags: tag bag; content: string;}
    let uuid = relation_of_function (fun p -> p.uuid)
    let author_uuid = relation_of_function (fun p -> p.author_uuid)
    let date = relation_of_function (fun p -> p.date)
    let title = relation_of_function (fun p -> p.title)
    let tags = relation_of_plural_function (fun p -> p.tags)
    let content = relation_of_function (fun p -> p.content)

    let author = author_uuid <=> (inverse Author.uuid)
  end

  module Comment = struct
    type t = { uuid: uuid; author_uuid: uuid; date: date; post_uuid: uuid; content: string;}
    let uuid = relation_of_function (fun c -> c.uuid)
    let author_uuid = relation_of_function (fun c -> c.author_uuid)
    let date = relation_of_function (fun c -> c.date)
    let post_uuid = relation_of_function (fun c -> c.post_uuid)
    let content = relation_of_function (fun c -> c.content)

    let author = author_uuid <=> (inverse Author.uuid)
    let post = post_uuid <=> (inverse Post.uuid)
  end

  type author = Author.t
  type post = Post.t
  type comment = Comment.t

  let posts = collection_of_list []
  let comments = collection_of_list []
  let authors = collection_of_list []
end
