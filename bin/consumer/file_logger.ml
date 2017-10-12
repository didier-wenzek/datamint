open Lwt.Infix

let open_append_only path =
  let flags = Unix.[O_WRONLY; O_NONBLOCK; O_APPEND; O_CREAT] in
  let perm = 0o640 in
  let mode = Lwt_io.output in
  Lwt_io.open_file ~flags ~perm ~mode path

let buffered_appender cout =
  let len = 1024 * 8 in
  let buffer = Buffer.create len in
  fun topic event ->
    Buffer.add_string buffer topic;
    Buffer.add_string buffer ": ";
    Buffer.add_string buffer event;
    if Buffer.length buffer > len
    then
      let content = Buffer.contents buffer in
      Buffer.clear buffer;
      Lwt_io.write cout content
    else
      Lwt.return_unit

let file path =
  open_append_only path
  >|=
  buffered_appender
