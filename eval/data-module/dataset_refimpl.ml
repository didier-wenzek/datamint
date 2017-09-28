module Bag : Dataset.S
  with type 'a t = 'a list
= struct
  type 'a t = 'a list

  let empty = []
  let singleton x = [x]
  let optional = function None -> [] | Some x -> [x]
  let merge = List.rev_append
  let add x xs = x::xs

  let map = List.map

  let filter = List.filter

  let flatmap f xs =
    xs 
    |> List.fold_left (fun acc x -> List.rev_append (f x) acc) []
    |> List.rev

  let unnest f make_pair =
    flatmap (fun x -> f x |> map (make_pair x))

  let exists = List.exists

  let flatten = List.concat

  let fold zero unit plus =
    let add acc x = plus acc (unit x) in
    List.fold_left add zero

  let aggregate init push merge =
    let add acc x = push x acc in
    fun xs -> List.fold_left add (init ()) xs

  let of_list xs = xs

  type ('a,'b) mapping = ('a,'b) Hashtbl.t
  let empty_mapping () = Hashtbl.create 1024
  let pairs m = Hashtbl.fold (fun k v acc -> (k,v)::acc) m []
  let keys m = Hashtbl.fold (fun k _ acc -> k::acc) m []
  let values m = Hashtbl.fold (fun _ v acc -> v::acc) m []
  let key_values = Hashtbl.find_all
  
end

