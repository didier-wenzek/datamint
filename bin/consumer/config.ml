type config = unit

let loggers config =
  Logger.Env.empty
  |> Logger.Env.set_default_logger (Some Logger.stderr)

let port config = 8000

let load path = ()
