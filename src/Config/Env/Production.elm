module Config.Env.Production exposing (env)

import Config.Env.Util exposing (Env, toContracts)
import Config.Network.Mainnet as Net


rpcSocketAddress : String
rpcSocketAddress =
    --rpcSocketAddressDataHub
    rpcSocketAddressAlchemy


rpcSocketAddressAlchemy : String
rpcSocketAddressAlchemy =
    "wss://polygon-mainnet.g.alchemy.com/v2/i9AGh_Sw3OXbep71XwSHZuNQUGvzVjJp"



--rpcSocketAddressDataHub : String
--rpcSocketAddressDataHub =
--    "wss://matic-mainnet--ws.datahub.figment.io/apikey/b40545ec4385320d4133b7e628a19480"


erc20 : String
erc20 =
    "0x264808855b0a6a5a318d238c6ee9f299179f27fc"


space : String
space =
    "0x9b71045ac2a1563dc5ff8e0c537413a6aae16cd1"


registry : String
registry =
    "0x8da7a7a48ebbd870358f2fd824e52e5142f44257"


snapper : String
snapper =
    "0x1363c844f9344c153f14671b12de40689c72533a"


env : Env
env =
    { name = "prod"
    , mapWidth = 1000
    , mapHeight = 1000
    , minZoom = 2
    , contracts = toContracts erc20 space registry snapper
    , rpcSocketAddress = rpcSocketAddress
    , rpcProvider = Net.rpcProvider
    , polyscanEndpoint = "https://polygonscan.com/"
    , snapshotUriPrefix = "https://d3ogaonsclhjen.cloudfront.net/"
    , playbackWindow = 65000
    , genesisSnapshotCid = "QmXPbeNeQGgYbm7zK9Gd9cziVeJHdSk9f8VtNTo25Ec3wi"
    , debug = False
    }
