{-
   Adapted from elm-ethereum-generator v4.0.0 generated file.

   https://github.com/cmditch/elm-ethereum-generator
-}


module Contract.Snapper exposing
    ( Cid
    , Snapshot
    , deltaLogsDecoder
    , latestSnapshot
    , snapshotDecoder
    )

import BigInt exposing (BigInt)
import Contract.Util exposing (unsafeBigIntToInt)
import Eth.Abi.Decode as AD
import Eth.Abi.Encode as AE
import Eth.Types exposing (Address, Call)
import Json.Decode as D exposing (Decoder)
import Regex


type alias Cid =
    String


type alias BlockNumber =
    Int


type alias Snapshot =
    { blockNumber : BlockNumber
    , cid : Cid
    }


type alias LatestSnapshotUgly =
    { shift : BigInt
    , latestSnapshotBlock : BigInt
    , shift2 : BigInt
    , latestSnapshotCid : String
    }


latestSnapshot : Address -> Call LatestSnapshotUgly
latestSnapshot contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AE.functionCall "69b18e3d" []
    , nonce = Nothing
    , decoder = latestSnapshotDecoder_
    }


latestSnapshotDecoder_ : Decoder LatestSnapshotUgly
latestSnapshotDecoder_ =
    AD.abiDecode LatestSnapshotUgly
        |> AD.andMap AD.uint
        |> AD.andMap AD.uint
        |> AD.andMap AD.uint
        |> AD.andMap AD.string
        |> AD.toElmDecoder


snapshotDecoder : Decoder Snapshot
snapshotDecoder =
    D.map toSnapshot latestSnapshotDecoder_


toSnapshot : LatestSnapshotUgly -> Snapshot
toSnapshot snapshotInfo =
    { blockNumber = snapshotInfo.latestSnapshotBlock |> unsafeBigIntToInt
    , cid = snapshotInfo.latestSnapshotCid |> trimCid
    }


trimCid : String -> String
trimCid s =
    -- FIXME: though safe, this is too ugly to be in production code
    -- Eth doesn't provide Tape constructor currently,
    -- we need a better way than this...
    s
        |> String.replace "." ""
        |> String.replace "@" ""
        |> Regex.replace
            (Maybe.withDefault Regex.never (Regex.fromString "\\0"))
            (\_ -> "")


playbackDeltaDecoder : Decoder Cid
playbackDeltaDecoder =
    D.at [ "data" ] (AD.string |> AD.toElmDecoder)


deltaLogsDecoder : Decoder (List Cid)
deltaLogsDecoder =
    D.list playbackDeltaDecoder
