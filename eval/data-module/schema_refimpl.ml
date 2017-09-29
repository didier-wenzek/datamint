open Util

module type S = sig
  include Schema.S

  type 'a dataset
  type ('a,'b) mapping

  val collection_of_dataset: 'a dataset -> 'a collection
  val collection_of_list: 'a list -> 'a collection
  val relation_of_function: ('a -> 'b) -> ('a,'b) relation
  val relation_of_plural_function: ('a -> 'b dataset) -> ('a,'b) relation
  val relation_of_mapping: ('a,'b) mapping -> ('a,'b) relation
  val relation_of_mapping_with_inv: ('a,'b) mapping -> ('b -> 'a) -> ('a,'b) relation
end

module Make(Dataset: Dataset.S): S
  with type 'a dataset = 'a Dataset.t
  and  type ('a,'b) mapping = ('a,'b) Dataset.mapping
= struct
  type 'a dataset = 'a Dataset.t
  type ('a,'b) mapping = ('a,'b) Dataset.mapping

  type ('a,'b) relation = {
    gen: 'c. (('a -> 'b -> 'c) -> 'c dataset) option;
    map: ('a -> 'b dataset) option;
    inv: ('b -> 'a dataset) option;
    chk: ('a -> 'b -> bool) option;
  }

  type 'a collection = (unit,'a) relation

  type 'a value = 'a dataset
  type ('a,'b) extractor = 'a -> 'b
  type ('a,'b,'c) injector = 'a -> 'b -> 'c

  let unit_value = Dataset.singleton ()

  let filter rel =
    Option.when_defined rel.chk $$ fun chk ->
    fun get_a get_b -> Dataset.filter (fun c -> chk (get_a c) (get_b c))

  let map rel =
    Option.when_defined rel.map $$ fun map ->
    fun get_a set_b -> Dataset.flatmap (fun c -> map (get_a c) |> Dataset.map (set_b c))

  let inv_map rel =
    Option.when_defined rel.inv $$ fun inv ->
    fun set_a get_b -> Dataset.flatmap (fun c -> inv (get_b c) |> Dataset.map (set_a c))

  let generate rel =
    Option.when_defined rel.gen $$ fun gen ->
    fun set_a set_b ->
      let pair c a b =
        let d = set_a c a 
        in set_b d b
      in
      Dataset.flatmap (fun c -> gen (pair c))

  let generate_members col =
    Option.when_defined (generate col) $$ fun generate ->
    fun set_a -> generate (fun x () -> x) set_a

  let filter_members col =
    Option.when_defined (filter col) $$ fun filter ->
    fun get_a -> filter (fun x -> ()) get_a

  let rel = {
    gen = None;
    map = None;
    inv = None;
    chk = None;
  }

  let rel_of_function make_dataset f = {
    rel with
    map = Some (fun x -> make_dataset (f x));
    chk = Some (fun x y -> Dataset.exists (fun x -> x = y) (make_dataset (f x)));
  }

  let eq = {
    rel with
    map = Some Dataset.singleton;
    inv = Some Dataset.singleton;
    chk = Some (=);
  }

  let ne = { rel with chk = Some (<>) }
  let lt = { rel with chk = Some (<) }
  let gt = { rel with chk = Some (>) }
  let le = { rel with chk = Some (<=) }
  let ge = { rel with chk = Some (>=) }

  let rel_of_col c = c
  let col_of_rel r = r

  let swap f a b = f b a

  let inverse rel = {
    gen = (match rel.gen with
      | None -> None
      | Some gen -> Some (fun make_pair -> gen (swap make_pair))
    );
    map = rel.inv;
    inv = rel.map;
    chk = Option.map swap rel.chk;
  }

  let (<=>) left right = {
    gen = (match left.gen, right.map with
      | Some gen_left, Some map_right ->
        Some (fun pack_ac -> gen_left (fun a b -> map_right b |> Dataset.map (pack_ac a)) |> Dataset.flatten)
      | _, _ -> (match left.inv, right.gen with
        | Some inv_left, Some gen_right ->
          Some (fun pack_ac -> gen_right (fun b c -> inv_left b |> Dataset.map ((swap pack_ac) c)) |> Dataset.flatten)
        | _, _ -> None
        )
    );
    map = (match left.map, right.map with
      | Some map_left, Some map_right ->
        Some (map_left >> Dataset.flatmap map_right)
      | _, _ -> None
    );
    inv = (match left.inv, right.inv with
      | Some inv_left, Some inv_right ->
        Some (inv_right >> Dataset.flatmap inv_left)
      | _, _ -> None
    );
    chk = (match left.map, right.chk with
      | Some map_left, Some chk_right ->
        Some (fun a c -> map_left a |> Dataset.exists (fun b -> chk_right b c))
      | _, _ -> (match left.chk, right.inv with
        | Some chk_left, Some inv_right ->
          Some (fun a c -> inv_right c |> Dataset.exists (chk_left a))
        | _, _ -> None
        )
    );
  }

  let relation_of_function f = {
    rel with
    map = Some (fun x -> Dataset.singleton (f x));
    chk = Some (fun x y -> f x = y);
  }

  let relation_of_plural_function f = {
    rel with
    map = Some f;
    chk = Some (fun x y -> Dataset.exists ((=) y) (f x));
  }

  let collection_of_dataset xs = {
    rel with
    gen = Some (fun make_pair -> Dataset.map (make_pair ()) xs);
    map = Some (fun () -> xs);
  }

  let collection_of_list xs =
    collection_of_dataset (Dataset.of_list xs)

  let relation_of_mapping m = {
    rel with
    gen = Some (fun make_pair -> Dataset.map (fun (k,v) -> make_pair k v) (Dataset.pairs m));
    map = Some (Dataset.key_values m);
    chk = Some (fun x y -> Dataset.exists ((=) y) (Dataset.key_values m x));
  }

  let relation_of_mapping_with_inv m f =
    let rel = relation_of_mapping m in {
      rel with
      inv = Some (fun x -> Dataset.singleton (f x))
    }

end
