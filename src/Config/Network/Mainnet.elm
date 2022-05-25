module Config.Network.Mainnet exposing (rpcProvider, rpcSocketAddress)

-- Rpc


alchemyEndPoint =
    "polygon-mainnet.g.alchemy.com/v2/i9AGh_Sw3OXbep71XwSHZuNQUGvzVjJp"


rpcSocketAddress =
    "wss://" ++ alchemyEndPoint


rpcHttpAddress =
    --"https://rpc-mainnet.maticvigil.com/"
    "https://polygon-rpc.com/"


blockExplorerUrls =
    "https://polygonscan.com"


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
