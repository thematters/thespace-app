module Config.Env.Staging exposing (env)

import Config.Env.Util exposing (Env, toContracts)
import Config.Network.Mumbai as Net


erc20 : String
erc20 =
    "0xeb6814043dc2184b0b321f6de995bf11bdbcc5b8"


space : String
space =
    "0xcd3f834dbfac270ead97d7bbb91e2d8012774c18"


registry : String
registry =
    "0x7edbb61ccea2f29e31525d33f310a9ea943c7a89"


snapper : String
snapper =
    "0x9553E6d62fdabA21Ee3bc8e63cDD59468954753c"


env : Env
env =
    { name = "stag"
    , mapWidth = 300
    , mapHeight = 300
    , minZoom = 3
    , contracts = toContracts erc20 space registry snapper
    , rpcSocketAddress = Net.rpcSocketAddress
    , rpcProvider = Net.rpcProvider
    , polyscanEndpoint = "https://mumbai.polygonscan.com/"
    , snapshotUriPrefix = "https://dnasxse9il3vg.cloudfront.net/"
    , debug = False
    }
