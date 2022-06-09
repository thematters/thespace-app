module Config.Env.Development exposing (env)

import Config.Env.Util exposing (Env, toContracts)
import Config.Network.Ganache as Net


rpcSocketAddress : String
rpcSocketAddress =
    "ws://127.0.0.1:8545"


erc20 : String
erc20 =
    "0xeb6814043dc2184b0b321f6de995bf11bdbcc5b8"


space : String
space =
    "0x410e494c14b75c371198d78dbb8e629bdf318e54"


registry : String
registry =
    "0x70bf60048d634173e29ceda160c0b4300bdba973"


snapper : String
snapper =
    "0xaad1d62d70995d8781ec78be717d7b48aa76fc1b"


env : Env
env =
    { name = "dev"
    , mapWidth = 1000
    , mapHeight = 1000
    , minZoom = 2
    , contracts = toContracts erc20 space registry snapper
    , rpcSocketAddress = rpcSocketAddress
    , rpcProvider = Net.rpcProvider
    , polyscanEndpoint = "https://mumbai.polygonscan.com/"
    , snapshotUriPrefix = "https://d35rfwwq3facyl.cloudfront.net/"
    , debug = True
    }
