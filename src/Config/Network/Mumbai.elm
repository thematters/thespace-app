module Config.Network.Mumbai exposing (rpcProvider)

import Config.Env.Util exposing (RpcProvider)


rpcHttpAddress : String
rpcHttpAddress =
    "https://rpc-mumbai.maticvigil.com/"


blockExplorerUrls : String
blockExplorerUrls =
    "https://mumbai.polygonscan.com"


rpcProvider : RpcProvider
rpcProvider =
    { chainId = 80001
    , chainName = "Matic Mumbai"
    , nativeCurrency =
        { name = "Matic Token"
        , symbol = "MATIC"
        , decimals = 18
        }
    , rpcUrls = [ rpcHttpAddress ]
    , blockExplorerUrls = [ blockExplorerUrls ]
    }
