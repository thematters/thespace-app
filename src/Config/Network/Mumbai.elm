module Config.Network.Mumbai exposing (rpcProvider, rpcSocketAddress)

import Config.Env.Util exposing (RpcProvider)


alchemyEndPoint : String
alchemyEndPoint =
    "polygon-mumbai.g.alchemy.com/v2/nz51PqwGNiZ4QUniU4fej3n1bqsIqOfC"


rpcSocketAddress : String
rpcSocketAddress =
    "wss://" ++ alchemyEndPoint


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
