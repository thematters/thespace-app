module Contract.Registry exposing (transferDecoder)

import Data exposing (RegistryTransferEvent, bigIntAsIntDecoder)
import Eth.Decode as ED
import Json.Decode as D


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
