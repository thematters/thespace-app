module Config.Env.Development exposing (env)

import Config.Env.Util exposing (Env, toContracts)
import Config.Network.Mumbai as Net


rpcSocketAddress : String
rpcSocketAddress =
    "wss://polygon-mumbai.g.alchemy.com/v2/nz51PqwGNiZ4QUniU4fej3n1bqsIqOfC"


erc20 : String
erc20 =
    "0x8bF1Fe40e5aD4f9ddD986ee6D53F2ABED326A52d"


space : String
space =
    "0x68f02A0552e6B9010F34680746cd17E9F98fEC65"


registry : String
registry =
    "0xe4DD2a5500C95ba86Ce1289dE5c35a383C6A9794"


snapper : String
snapper =
    "0xf496a86c20689f263af14ab2e59fd1296df71c27"


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
    , snapshotUriPrefix = "https://d1gykh5008m3d7.cloudfront.net/"
    , playbackWindow = 10000
    , genesisSnapshotCid = "QmXPbeNeQGgYbm7zK9Gd9cziVeJHdSk9f8VtNTo25Ec3wi"
    , debug = True
    }
