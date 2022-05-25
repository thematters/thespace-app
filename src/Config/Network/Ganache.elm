module Config.Network.Ganache exposing (rpcProvider, rpcSocketAddress)

-- Rpc


rpcSocketAddress =
    "ws://127.0.0.1:8545"


rpcHttpAddress =
    "http://127.0.0.1:8545"


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
