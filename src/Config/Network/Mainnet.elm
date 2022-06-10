module Config.Network.Mainnet exposing (rpcProvider)

import Config.Env.Util exposing (RpcProvider)


rpcHttpAddress : String
rpcHttpAddress =
    "https://polygon-rpc.com"


blockExplorerUrls : String
blockExplorerUrls =
    "https://polygonscan.com"


rpcProvider : RpcProvider
rpcProvider =
    { chainId = 137
    , chainName = "Polygon Mainnet"
    , nativeCurrency =
        { name = "Matic Token"
        , symbol = "MATIC"
        , decimals = 18
        }
    , rpcUrls = [ rpcHttpAddress ]
    , blockExplorerUrls = [ blockExplorerUrls ]
    }
