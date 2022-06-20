module Config.Env.Util exposing (Contracts, Env, RpcProvider, toContracts)

import Eth.Types exposing (Address)
import Eth.Utils exposing (unsafeToAddress)


type alias Env =
    { name : String
    , debug : Bool
    , mapWidth : Int
    , mapHeight : Int
    , minZoom : Float
    , contracts : Contracts
    , rpcSocketAddress : String
    , rpcProvider : RpcProvider
    , polyscanEndpoint : String
    , playbackWindow : Int
    , snapshotUriPrefix : String
    }


type alias Contracts =
    { erc20 : Address
    , space : Address
    , registry : Address
    , snapper : Address
    }


type alias RpcProvider =
    { chainId : Int
    , chainName : String
    , nativeCurrency : { name : String, symbol : String, decimals : Int }
    , rpcUrls : List String
    , blockExplorerUrls : List String
    }


toContracts : String -> String -> String -> String -> Contracts
toContracts erc20 space registry snapper =
    { erc20 = erc20 |> unsafeToAddress
    , space = space |> unsafeToAddress
    , registry = registry |> unsafeToAddress
    , snapper = snapper |> unsafeToAddress
    }
