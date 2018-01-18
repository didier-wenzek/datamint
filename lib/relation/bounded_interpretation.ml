module type Col = sig
  type 'a t
  type ('a,'b) reducer
  type ('a,'b) mapping

  val singleton: 'a -> 'a t
  val optional: 'a option -> 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val filter: ('a -> bool) -> 'a t -> 'a t
  val flatmap: ('a -> 'b t) -> 'a t -> 'b t
  val unnest: ('a -> 'b t) -> 'a t -> 'a * 'b t

  val reduce: ('a,'b) reducer -> 'a t -> 'b

  val pairs: ('a,'b) mapping -> ('a * 'b) t
  val keys: ('a,'b) mapping -> 'a t
  val values_of_key: ('a,'b) mapping -> 'a -> 'b t
  val grouping_reducer: ('a -> 'k) -> ('a -> 'v) -> ('a, ('k,'v) mapping) reducer
  val mapping_reducer: ('a -> 'k) -> ('a -> 'v) -> ('v, 'w t) reducer -> ('a, ('k,'w) mapping) reducer

  val member: 'a t -> 'a -> bool
  val related: ('a,'b) mapping -> 'a -> 'b -> bool
end

module Make(Col: Col) : Interpretation.S
  with type 'a records = 'a Col.t
= struct

  (** A relation is represented by optional functions
      to generate, map and filter the pairs of the relation. *)
  type ('a,'b,'p_gen,'a_gen,'b_gen) relation = {
    gen: (('a*'b)  Col.t, 'p_gen) Capability.t;
    map: ('a -> 'b Col.t, 'a_gen) Capability.t;
    inv: ('b -> 'a Col.t, 'b_gen) Capability.t;
    chk: 'a -> 'b -> bool;
  }

  (** A collection is represented by optional functions
      to generate and filter the elements of the collection. *)
  type ('a,'a_gen) collection = {
    gen_mem: ('a Col.t, 'a_gen) Capability.t;
    chk_mem: 'a -> bool;
  }

  (** A stream of records is materialized by a bounded collection *)
  type 'a records = 'a Col.t

  (** An [('a,'b) reducer] reduces a stream of ['a] values into a collection of ['b] values. *)
  type ('a,'b) reducer = ('a,'b Col.t) Col.reducer

  (* Extract a field of a record. *)
  type ('a,'b) extractor = 'a -> 'b

  (* Inject a field value into a record, making an extended record. *)
  type ('a,'b,'c) injector = 'a -> 'b -> 'c

  let record_source  = Col.singleton ()

  let generate rel =
    let gen = Capability.get rel.gen in
    fun set_a set_b ->
      let pair c (a,b) =
        let d = set_a c a 
        in set_b d b
      in
      Col.flatmap (fun c -> Col.map (pair c) gen)

  let map rel =
    let map = Capability.get rel.map in
    fun get_a set_b -> Col.flatmap (fun c -> map (get_a c) |> Col.map (set_b c))

  let inv_map rel =
    let inv = Capability.get rel.inv in
    fun set_a get_b -> Col.flatmap (fun c -> inv (get_b c) |> Col.map (set_a c))

  let filter rel =
    let chk = rel.chk in
    fun get_a get_b -> Col.filter (fun c -> chk (get_a c) (get_b c))

  let generate_member col =
    let gen = Capability.get col.gen_mem in
    fun set_a -> Col.flatmap (fun c -> Col.map (set_a c) gen)

  let filter_member col =
    let chk = col.chk_mem in
    fun get_a -> Col.filter (fun c -> chk (get_a c))

  let rel_of_col col = {
    gen = Capability.map (fun elts -> Col.map (fun e -> ((),e)) elts) col.gen_mem;
    map = Capability.map (fun elts () -> elts) col.gen_mem;
    inv = Capability.some (fun a -> Col.optional (if col.chk_mem a then Some () else None));
    chk = (fun () a -> col.chk_mem a);
  }

  let col_of_rel rel = {
    gen_mem = Capability.map (fun elts -> elts ()) rel.map;
    chk_mem = (fun a -> rel.chk () a);
  }

  let rel_of_mapping m = {
    gen = Capability.some (Col.pairs m);
    map = Capability.some (Col.values_of_key m);
    inv = Capability.some (fun v -> Col.filter (fun k -> Col.member (Col.values_of_key m k) v) (Col.keys m));
    chk = Col.related m;
  }

  let col_of_col col = {
    gen_mem = Capability.some col;
    chk_mem = Col.member col;
  }

  let reduce red get_v =
    fun c -> c |> Col.map get_v |> Col.reduce red |> col_of_col

  let group get_k get_v =
    fun c -> c |> Col.reduce (Col.grouping_reducer get_k get_v) |> rel_of_mapping

  let group_reduce red get_k get_v =
    fun c -> c |> Col.reduce (Col.mapping_reducer get_k get_v red) |> rel_of_mapping

end
