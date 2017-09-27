module Compile(Schema: Blog_schema.S) = struct
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

  let posts_of_tag tag =
    let post, title, author, date = var4 () in
    query [
      !! post Post.tags (value tag);
      !! post (Post.author <=> Author.name) author;
      !! post Post.date date;
      !! post Post.title title;
    ]
    
  let authors_commenting_their_posts =
    let author = var1 () in
    query [
      !! author (Author.posts <=> Post.comments <=> Comment.author) author
    ]
end
