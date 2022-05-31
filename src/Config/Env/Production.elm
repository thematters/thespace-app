module Config.Env.Production exposing (env)

import Config.Env.Util exposing (Env, toContracts)
import Config.Network.Mainnet as Net


erc20 : String
erc20 =
    "0x264808855b0a6a5a318d238c6ee9f299179f27fc"


space : String
space =
    "0xa84108b2d248e0991b5c214db29ceab989f0bea1"


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
    , rpcSocketAddress = Net.rpcSocketAddress
    , rpcProvider = Net.rpcProvider
    , polyscanEndpoint = "https://polygonscan.com/"
    , snapshotUriPrefix = "https://d3ogaonsclhjen.cloudfront.net/"
    , debug = False

    --, debug = True
    }
