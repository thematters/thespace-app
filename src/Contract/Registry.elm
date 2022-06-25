module Contract.Registry exposing (RegistryTransferEvent, transferDecoder)

import Contract.Util exposing (bigIntAsIntDecoder)
import Eth.Decode as ED
import Eth.Types exposing (Address)
import Json.Decode as D


type alias RegistryTransferEvent =
    { blockNumber : Int
    , from : Address
    , to : Address
    , index : Int
    , removed : Bool
    }


transferDecoder : D.Decoder RegistryTransferEvent
transferDecoder =
    D.map5 RegistryTransferEvent
        (D.at [ "blockNumber" ] ED.hexInt)
        -- from
        (D.at [ "topics" ] (D.index 1 ED.address))
        -- to
        (D.at [ "topics" ] (D.index 2 ED.address))
        -- index
        (D.at [ "topics" ] (D.index 3 bigIntAsIntDecoder))
        (D.at [ "removed" ] D.bool)
