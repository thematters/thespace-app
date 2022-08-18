module Config.Env.Dali exposing (env)

import Config.Env.Util exposing (Env, toContracts)
import Config.Network.Mainnet as Net


rpcSocketAddress : String
rpcSocketAddress =
    rpcSocketAddressAlchemy


rpcSocketAddressAlchemy : String
rpcSocketAddressAlchemy =
    "wss://polygon-mainnet.g.alchemy.com/v2/i9AGh_Sw3OXbep71XwSHZuNQUGvzVjJp"


erc20 : String
erc20 =
    "0x264808855b0a6a5a318d238c6ee9f299179f27fc"


space : String
space =
    "0x66a53db6565f4950c72430f3e7510e5f2af81d34"


registry : String
registry =
    "0x76b53c7cc68122f3db498b50df1d1316817e693b"


snapper : String
snapper =
    "0xcd8235aa7e266cb4c839472b2d454231f3a4caee"


env : Env
env =
    { name = "prod"
    , mapWidth = 100
    , mapHeight = 100
    , minZoom = 6
    , contracts = toContracts erc20 space registry snapper
    , rpcSocketAddress = rpcSocketAddress
    , rpcProvider = Net.rpcProvider
    , polyscanEndpoint = "https://polygonscan.com/"
    , snapshotUriPrefix = "https://d17zdvjcjjb6x6.cloudfront.net/"
    , playbackWindow = 45000
    , genesisSnapshotCid = "QmeZYYJUSbiL6ffDdfnjCXNcwADNAnecqefutkvmUkG58p"
    , debug = False
    }
