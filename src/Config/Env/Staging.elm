module Config.Env.Staging exposing (env)

import Config.Env.Util exposing (Env, toContracts)
import Config.Network.Mainnet as Net


rpcSocketAddress : String
rpcSocketAddress =
    --rpcSocketAddressDataHub
    rpcSocketAddressAlchemy


rpcSocketAddressAlchemy : String
rpcSocketAddressAlchemy =
    "wss://polygon-mainnet.g.alchemy.com/v2/NLv95hqjMltinp-8MKwMYyJphTqn2YAp"



--rpcSocketAddressDataHub : String
--rpcSocketAddressDataHub =
--    "wss://matic-mainnet--ws.datahub.figment.io/apikey/e62a4e148110be81b9e4ebe76e231558"


erc20 : String
erc20 =
    "0x41d4c1973d3f582439701bdfdd3d2c99130acab0"


space : String
space =
    "0x4ba11d0758f5feb7705ce9b38b00b61c7ab8667e"


registry : String
registry =
    "0xaf1dfe31de25216501fc65719997a7163f8b7ee4"


snapper : String
snapper =
    "0x22b2a05fabe0cd6b728465ff4cae219149ef1818"


env : Env
env =
    { name = "stag"
    , mapWidth = 1000
    , mapHeight = 1000
    , minZoom = 2
    , contracts = toContracts erc20 space registry snapper
    , rpcSocketAddress = rpcSocketAddress
    , rpcProvider = Net.rpcProvider
    , polyscanEndpoint = "https://polygonscan.com/"
    , snapshotUriPrefix = "https://d23t3m1s3moj89.cloudfront.net/"
    , playbackWindow = 300
    , genesisSnapshotCid = "Qmf9D3TQQ8twZ9igqoaFtQHshp2JoA54xVi3RGU6Q7uAoE"
    , debug = True
    }
