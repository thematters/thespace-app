module Config.Env.Production exposing (env)

import Config.Env.Util exposing (Env, toContracts)
import Config.Network.Mainnet as Net


erc20 : String
erc20 =
    "0x41d4c1973d3f582439701bdfdd3d2c99130acab0"


space : String
space =
    "0x06110bddbddf24bdb89c56c8ef4bfb092cd52cdb"


registry : String
registry =
    "0xaf1dfe31de25216501fc65719997a7163f8b7ee4"


snapper : String
snapper =
    "0x22b2a05fabe0cd6b728465ff4cae219149ef1818"


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
    , snapshotUriPrefix = "https://d23t3m1s3moj89.cloudfront.net/"
    , debug = False

    --, debug = True
    }
