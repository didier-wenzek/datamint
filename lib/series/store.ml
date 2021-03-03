open Lwt.Infix

type path = Lwt_io.file_name

type 'a store_hdl = {
  (* Return the actual content or the given seed. *)
  read_content: 'a -> 'a Lwt.t;

  (* Push all the updates and return a value reflecting the current state. *)
  write_content: 'a -> 'a Lwt.t;
}

type 'a view_hdl = {
  (* Atomically update the current store content with the given value. *)
  update_content: 'a -> unit Lwt.t;
}

type 'a log_hdl =
  Logger: {
    init_buffer: 'b option -> 'b;
    push_buffer: 'a -> 'b -> 'b;
    write_buffer: 'b -> unit Lwt.t; 
  } -> 'a log_hdl

type 'a store = path -> 'a store_hdl
type 'a view = path -> 'a view_hdl
type 'a log = path -> 'a log_hdl

type 'a file_encoding = {
  decode: Lwt_io.input_channel -> 'a Lwt.t;
  encode: 'a -> Lwt_io.output_channel -> unit Lwt.t;
}

type 'a store_combiner =
  | Combiner: ('a store * 'b store * (('a -> 'b -> 'c) * ('c -> 'a) * ('c -> 'b))) -> 'c store_combiner

let marshal_encoding : 'a. 'a file_encoding = {
  decode = Lwt_io.read_value;
  encode = fun x cout -> Lwt_io.write_value cout x;
}

let string_line_encoding = {
  decode = Lwt_io.read_line;
  encode = fun x cout -> Lwt_io.write_line cout x;
}

let line_encoding encode decode = {
  decode = (fun cin -> Lwt_io.read_line cin >|= fun s -> decode s);
  encode = (fun x cout -> encode x |> Lwt_io.write_line cout);
}

let pair_encoding head tail = {
  decode = (fun cin -> head.decode cin >>= fun h -> tail.decode cin >|= fun t -> (h,t));
  encode = (fun (h,t) cout -> head.encode h cout >>= fun () -> tail.encode t cout);
}

let return_default_if_noent read default =
  Lwt.catch read (function
    | Unix.Unix_error (Unix.ENOENT, _,_) -> Lwt.return default
    | exn -> Lwt.fail exn)

let to_file encoding path =
  let read_content () =
    Lwt_io.(with_file ~mode:Input path encoding.decode)
  in
  let write_content state =
    Lwt_io.(with_file ~mode:Output path (encoding.encode state))
    >>= fun () ->
    Lwt.return state
  in
  {
    read_content = read_content |> return_default_if_noent;
    write_content;
  }

let buffered_logger encode write =
  let init_buffer = function
    | None -> Buffer.create 1024
    | Some buff -> Buffer.clear buff; buff
  in
  let push_buffer str buff =
    Buffer.add_string buff (encode str);
    Buffer.add_char buff '\n';
    buff
  in
  let write_buffer buff =
    write (Buffer.contents buff)
  in
  Logger {
    init_buffer;
    push_buffer;
    write_buffer;
  }

let write_str str cout =
  Lwt_io.write_from_string_exactly cout str 0 (String.length str)

let write_str_stdout str =
  write_str str Lwt_io.stdout

let console_logger encode _ignored_path = 
  buffered_logger encode write_str_stdout

let file_logger encode file_path dir_path =
  let path = Filename.concat dir_path file_path in
  let append str =
    Lwt_io.(with_file ~flags:Unix.[O_WRONLY;O_CREAT;O_APPEND] ~mode:Output path (write_str str))
  in
  buffered_logger encode append

let file_view encode file_path dir_path =
  let path = Filename.concat dir_path file_path in
  let update_content a =
    Lwt_io.(with_file ~flags:Unix.[O_WRONLY;O_CREAT] ~mode:Output path (write_str (encode a)))
  in
  { update_content }

let marshal_store file_path dir_path =
  let path = Filename.concat dir_path file_path in
  to_file marshal_encoding path

let map_lwt f =
  function
    | Unbounded.Done a -> f a >|= fun b -> Process.Done b
    | Unbounded.Continue a -> f a >|= fun b -> Process.Continue b

let store_unbounded_gen state_store view_store path gen =
  let open Unbounded in
  let state_store = state_store path in
  let write_state = match view_store with
    | None -> state_store.write_content
    | Some view_store -> (
      let view_store = view_store path in
      fun state ->
        gen.view state
        >>= fun view ->
        view_store.update_content view
        >>= fun () ->
        state_store.write_content state
    )
  in
  let step state =
    gen.next state
    >>=
    map_lwt write_state
  in
  state_store.read_content gen.seed
  >>= Process.loop_until_done step

let log_unbounded_series state_store logger path xs =
  let open Unbounded in
  let state_store = state_store path in
  let Logger logger = logger path in
  let buff = logger.init_buffer None in
  let reducer = Reducer.{
    seed = logger.init_buffer (Some buff);
    push = logger.push_buffer;
    term = (fun buff -> buff);
    full_check = None;
  } in
  let gen = generate xs reducer in
  let read_state (seed,buff) =
    state_store.read_content seed
    >|= fun state ->
    (state,buff)
  in
  let write_state (state,buff) =
    logger.write_buffer buff
    >>= fun () ->
    state_store.write_content state
    >|= fun state ->
    (state, logger.init_buffer (Some buff))
  in
  let step state =
    gen.next state
    >>=
    map_lwt write_state
  in
  read_state gen.seed
  >>= Process.loop_until_done step

let product_store a_store b_store =
  let read_content (a_default, b_default) =
    a_store.read_content a_default
    >>= fun a ->
    b_store.read_content b_default
    >>= fun b ->
    Lwt.return (a,b)
  in
  let write_content (a,b) =
    a_store.write_content a
    >>= fun a ->
    b_store.write_content b
    >>= fun b ->
    Lwt.return (a,b)
  in
  { read_content;
    write_content;
  }

let combine_store a_store b_store =
  fun path -> product_store (a_store path) (b_store path)

let unit_store _path =
  let nop () = Lwt.return_unit in
  { read_content = nop;
    write_content = nop;
  }

let take_left = (
  (fun left () -> left),
  (fun left -> left),
  (fun _left -> ()))

let take_right = (
  (fun () right -> right),
  (fun _right -> ()),
  (fun right -> right))

let take_left_tail (comb,extract_left,extract_right) = (
  (fun (left, tail) (right, ()) -> (comb left right, tail)),
  (fun (head, tail) -> (extract_left head, tail)),
  (fun (head, _tail) -> (extract_right head, ())))

let take_right_tail (comb,extract_left,extract_right) = (
  (fun (left, ()) (right, tail) -> (comb left right, tail)),
  (fun (head, _tail) -> (extract_left head, ())),
  (fun (head, tail) -> (extract_right head, tail)))

let store_of_combiner = function 
  | Combiner (left_store, right_store, (combine, extract_left, extract_right)) ->
    fun path ->
      let left_store = left_store path in
      let right_store = right_store path in
      let read_content c =
        left_store.read_content (extract_left c)
        >>= fun a ->
        right_store.read_content (extract_right c)
        >>= fun b ->
        Lwt.return (combine a b)
      in
      let write_content c =
        left_store.write_content (extract_left c)
        >>= fun a ->
        right_store.write_content (extract_right c)
        >>= fun b ->
        Lwt.return (combine a b)
      in
      { read_content;
        write_content;
      }

let combine_log l g path =
  let Logger l = l path in
  let Logger g = g path in
  let init_buffer = function
    | None ->
      (l.init_buffer None, g.init_buffer None)
    | Some (l_buff, g_buff) ->
      (l.init_buffer (Some l_buff), g.init_buffer (Some g_buff))
  in
  let push_buffer x (l_buff, g_buff) =
    (l.push_buffer x l_buff, g.push_buffer x g_buff)
  in
  let write_buffer (l_buff, g_buff) =
    l.write_buffer l_buff
    >>= fun () ->
    g.write_buffer g_buff
  in
  Logger {
    init_buffer;
    push_buffer;
    write_buffer;
  }

let combine_view v w path =
  let v = v path in
  let w = w path in
  let update_content a =
    v.update_content a
    >>= fun () ->
    w.update_content a
  in
  { update_content }
