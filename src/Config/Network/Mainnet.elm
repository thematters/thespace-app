module Config.Network.Mainnet exposing (rpcProvider, rpcSocketAddress)

import Config.Env.Util exposing (RpcProvider)


alchemyEndPoint : String
alchemyEndPoint =
    "polygon-mainnet.g.alchemy.com/v2/i9AGh_Sw3OXbep71XwSHZuNQUGvzVjJp"


rpcSocketAddress : String
rpcSocketAddress =
    "wss://" ++ alchemyEndPoint


rpcHttpAddress : String
rpcHttpAddress =
    "https://polygon-rpc.com/"


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
