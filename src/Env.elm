module Env exposing (env)

--import Config.Env.Staging as Env
--import Config.Env.Production as Env
--import Config.Env.Development as Env

import Config.Env.Dali as Env
import Config.Env.Util exposing (Env)


env : Env
env =
    let
        env_ =
            Env.env
    in
    --env_
    --{ env_ | debug = False }
    { env_ | debug = True }
