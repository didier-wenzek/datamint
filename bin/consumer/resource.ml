module Dict = Map.Make(String)

type name = string

type 'a t = {
  values: 'a Dict.t;
  default: 'a option;
}

type resource =
  | Logger of Logger.logger
  | Publisher of Publisher.publisher

let empty = {
  values = Dict.empty;
  default = None;
}

let add_resource name value env = {
  env with
  values = Dict.add name value env.values
}

let set_default default env = {
  env with default;
}

let wild_card = "*"

let add_resource_or_default name value env =
  if name = wild_card
  then set_default (Some value) env
  else add_resource name value env

let find env t =
  try
    Some (Dict.find t env.values)
  with
    Not_found -> env.default

open Lwt.Infix

let add_resources open_resource =
  let add_resource env (name,res_config) =
    open_resource res_config
    >|= fun res ->
    add_resource_or_default name res env
  in
  let rec loop confs env = match confs with
    | [] -> Lwt.return env
    | conf::confs -> (
      add_resource env conf
      >>=
      loop confs
    )
  in
  loop

let open_logger conf =
  Logger.make_logger conf
  >|= fun l ->
  Logger l

let open_publisher conf =
  Publisher.make_publisher conf
  >|= fun p ->
  Publisher p

let open_resources loggers publishers =
  empty
  |>  add_resources open_logger loggers
  >>= add_resources open_publisher publishers
