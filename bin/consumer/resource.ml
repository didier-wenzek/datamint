module Dict = Map.Make(String)
open Lwt.Infix

type name = string

type t =
  | Logger of Logger.logger
  | Publisher of Publisher.publisher

let wild_card = "*"

module Env = struct 
  type 'a t = {
    values: 'a Dict.t;
    default: 'a option;
  }

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

  let add_resource_or_default name value env =
    if name = wild_card
    then set_default (Some value) env
    else add_resource name value env

  let find env t =
    try
      Some (Dict.find t env.values)
    with
      Not_found -> env.default

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
end

let open_logger conf =
  Logger.make_logger conf
  >|= fun l ->
  Logger l

let open_publisher conf =
  Publisher.make_publisher conf
  >|= fun p ->
  Publisher p

let open_resources loggers publishers =
  Env.empty
  |>  Env.add_resources open_logger loggers
  >>= Env.add_resources open_publisher publishers
