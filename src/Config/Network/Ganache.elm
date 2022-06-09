module Config.Network.Ganache exposing (rpcProvider)

import Config.Env.Util exposing (RpcProvider)


rpcHttpAddress : String
rpcHttpAddress =
    "http://127.0.0.1:8545"


rpcProvider : RpcProvider
rpcProvider =
    { chainId = 1337
    , chainName = "Genache"
    , nativeCurrency =
        { name = "TheSpace Token"
        , symbol = "SPACE"
        , decimals = 18
        }
    , rpcUrls = [ rpcHttpAddress ]
    , blockExplorerUrls = []
    }
