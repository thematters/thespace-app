module Config.Network.Mumbai exposing (rpcProvider, rpcSocketAddress)

-- Rpc


alchemyEndPoint =
    "polygon-mumbai.g.alchemy.com/v2/nz51PqwGNiZ4QUniU4fej3n1bqsIqOfC"


rpcSocketAddress =
    "wss://" ++ alchemyEndPoint


rpcHttpAddress =
    "https://rpc-mumbai.maticvigil.com/"


blockExplorerUrls =
    "https://mumbai.polygonscan.com"


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
