module Config.Env.Production exposing (env)

import Config.Env.Util exposing (Env, toContracts)
import Config.Network.Mainnet as Net


erc20 : String
erc20 =
    "0x41d4c1973d3f582439701bdfdd3d2c99130acab0"


space : String
space =
    "0xc956a1da0be768eac0d59be19d6762ec094f1566"


registry : String
registry =
    "0x1ff7500668f88ac71b108e2ad1dfa5bf44d617cb"


snapper : String
snapper =
    "0x12103bb80c09d9dd66a771cc7b94808ab110e77c"


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
