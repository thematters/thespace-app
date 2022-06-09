module Config.Network.Ganache exposing (rpcProvider, rpcSocketAddress)

import Config.Env.Util exposing (RpcProvider)


rpcSocketAddress : String
rpcSocketAddress =
    "ws://127.0.0.1:8545"


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
