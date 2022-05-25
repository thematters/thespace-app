{-
   Adapted from elm-ethereum-generator v4.0.0 generated file.

   https://github.com/cmditch/elm-ethereum-generator
-}


module Contract.Snapper exposing
    ( LatestSnapshotInfo
    , latestSnapshotInfo
    , latestSnapshotInfoDecoder
    , trimCid
    )

import BigInt exposing (BigInt)
import Eth.Abi.Decode as AD
import Eth.Abi.Encode as AE
import Eth.Types exposing (Address, Call)
import Json.Decode exposing (Decoder)
import Regex


type alias LatestSnapshotInfo =
    { shift : BigInt
    , latestSnapshotBlock : BigInt
    , shift2 : BigInt
    , latestSnapshotCid : String
    }


latestSnapshotInfo : Address -> Call LatestSnapshotInfo
latestSnapshotInfo contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AE.functionCall "69b18e3d" []
    , nonce = Nothing
    , decoder = latestSnapshotInfoDecoder
    }


latestSnapshotInfoDecoder : Decoder LatestSnapshotInfo
latestSnapshotInfoDecoder =
    AD.abiDecode LatestSnapshotInfo
        |> AD.andMap AD.uint
        |> AD.andMap AD.uint
        |> AD.andMap AD.uint
        |> AD.andMap AD.string
        |> AD.toElmDecoder


trimCid : String -> String
trimCid s =
    -- FIXME: this is fucking ugly...
    -- due to Eth doesn't provide Tape type...
    -- we need a better way...
    s
        |> String.replace "." ""
        |> String.replace "@" ""
        |> Regex.replace
            (Maybe.withDefault Regex.never (Regex.fromString "\\0"))
            (\_ -> "")
