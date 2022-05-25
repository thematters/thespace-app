{-
   Adapted from elm-ethereum-generator v4.0.0 generated file.

   https://github.com/cmditch/elm-ethereum-generator
-}


module Contract.TheSpace exposing
    ( GetPixel
    , GetPixelsByOwner
    , bid
    , colorDecoder
    , colorLogsDecoder
    , getPixel
    , getPixelDecoder
    , getPixelsByOwner
    , getPixelsByOwnerDecoder
    , mintTaxIndex
    , priceDecoder
    , priceLogsDecoder
    , setColor
    , setPixel
    , setPrice
    , taxConfig
    , taxDecoder
    , taxLogsDecoder
    , taxRateIndex
    , transferDecoder
    , transferLogsDecoder
    , treasuryShareIndex
    , ubiDecoder
    , ubiLogsDecoder
    , withdrawUbi
    )

import BigInt exposing (BigInt)
import Data
    exposing
        ( ColorEvent
        , Price
        , PriceEvent
        , TaxEvent
        , TransferEvent
        , UbiEvent
        , bigIntAsIntDecoder
        )
import Eth.Abi.Decode as AD
import Eth.Abi.Encode as AE
import Eth.Decode as ED
import Eth.Types exposing (Address, Call)
import Json.Decode as D



-- TaxConfig


taxRateIndex : BigInt
taxRateIndex =
    BigInt.fromInt 0


treasuryShareIndex : BigInt
treasuryShareIndex =
    BigInt.fromInt 1


mintTaxIndex : BigInt
mintTaxIndex =
    BigInt.fromInt 2


taxConfig : Address -> BigInt -> Call BigInt
taxConfig contractAddress a_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AE.functionCall "e7d6fbbf" [ AE.uint a_ ]
    , nonce = Nothing
    , decoder = AD.toElmDecoder AD.uint
    }



-- Calls


getPixel : Address -> BigInt -> Call GetPixel
getPixel contractAddress tokenId_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AE.functionCall "b4f80eb9" [ AE.uint tokenId_ ]
    , nonce = Nothing
    , decoder = getPixelDecoder
    }


getPixelsByOwner : Address -> Address -> BigInt -> BigInt -> Call GetPixelsByOwner
getPixelsByOwner contractAddress owner limit offset =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data =
        Just <|
            AE.functionCall "c0aed4a7"
                [ AE.address owner, AE.uint limit, AE.uint offset ]
    , nonce = Nothing
    , decoder = getPixelsByOwnerDecoder
    }


type alias GetPixel =
    { id : BigInt
    , price : BigInt
    , lastTaxBK : BigInt
    , ubi : BigInt
    , owner : Address
    , color : BigInt
    }


type alias GetPixelsByOwner =
    { total : BigInt
    , limit : BigInt
    , offset : BigInt
    , pixels : List GetPixel
    }


getPixelDecoder_ : AD.AbiDecoder GetPixel
getPixelDecoder_ =
    AD.abiDecode GetPixel
        |> AD.andMap AD.uint
        |> AD.andMap AD.uint
        |> AD.andMap AD.uint
        |> AD.andMap AD.uint
        |> AD.andMap AD.address
        |> AD.andMap AD.uint


getPixelDecoder : D.Decoder GetPixel
getPixelDecoder =
    getPixelDecoder_ |> AD.toElmDecoder


getPixelsByOwnerDecoder : D.Decoder GetPixelsByOwner
getPixelsByOwnerDecoder =
    AD.abiDecode GetPixelsByOwner
        |> AD.andMap AD.uint
        |> AD.andMap AD.uint
        |> AD.andMap AD.uint
        |> AD.andMap (AD.dynamicArray getPixelDecoder_)
        |> AD.toElmDecoder


setPixel : Address -> Address -> BigInt -> BigInt -> BigInt -> BigInt -> Call ()
setPixel contractAddress sender tokenId_ bid__ price_ color_ =
    { to = Just contractAddress
    , from = Just sender
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data =
        Just <|
            AE.functionCall "ff1644d8"
                [ AE.uint tokenId_
                , AE.uint bid__
                , AE.uint price_
                , AE.uint color_
                ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


withdrawUbi : Address -> Address -> BigInt -> Call ()
withdrawUbi contractAddress collector tokenId__ =
    { to = Just contractAddress
    , from = Just collector
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AE.functionCall "ee1ef07e" [ AE.uint tokenId__ ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


bid : Address -> BigInt -> BigInt -> Call ()
bid contractAddress tokenId__ price__ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data =
        Just <|
            AE.functionCall "598647f8"
                [ AE.uint tokenId__, AE.uint price__ ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


setColor : Address -> BigInt -> BigInt -> Call ()
setColor contractAddress tokenId_ color_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data =
        Just <|
            AE.functionCall "242d81cd"
                [ AE.uint tokenId_, AE.uint color_ ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


setPrice : Address -> BigInt -> BigInt -> Call ()
setPrice contractAddress tokenId__ price__ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data =
        Just <|
            AE.functionCall "f7d97577"
                [ AE.uint tokenId__, AE.uint price__ ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }



-- Event Decoders


colorDecoder : D.Decoder ColorEvent
colorDecoder =
    D.map5 ColorEvent
        (D.at [ "blockNumber" ] ED.hexInt)
        -- index
        (D.at [ "topics" ] (D.index 1 bigIntAsIntDecoder))
        -- owner
        (D.at [ "topics" ] (D.index 3 ED.address))
        -- color
        (D.at [ "topics" ] (D.index 2 bigIntAsIntDecoder))
        (D.at [ "removed" ] D.bool)


priceDecoder : D.Decoder PriceEvent
priceDecoder =
    D.map5 PriceEvent
        (D.at [ "blockNumber" ] ED.hexInt)
        -- index
        (D.at [ "topics" ] (D.index 1 bigIntAsIntDecoder))
        -- owner
        (D.at [ "topics" ] (D.index 2 ED.address))
        -- price
        (D.at [ "data" ] ED.bigInt)
        (D.at [ "removed" ] D.bool)


transferDecoder : D.Decoder TransferEvent
transferDecoder =
    D.map6 TransferEvent
        (D.at [ "blockNumber" ] ED.hexInt)
        -- index
        (D.at [ "topics" ] (D.index 1 bigIntAsIntDecoder))
        -- from
        (D.at [ "topics" ] (D.index 2 ED.address))
        -- to
        (D.at [ "topics" ] (D.index 3 ED.address))
        -- amount
        (D.at [ "data" ] ED.bigInt)
        (D.at [ "removed" ] D.bool)


taxUbiDecoder : (Int -> Int -> Address -> Price -> Bool -> value) -> D.Decoder value
taxUbiDecoder t =
    D.map5 t
        (D.at [ "blockNumber" ] ED.hexInt)
        -- index
        (D.at [ "topics" ] (D.index 1 bigIntAsIntDecoder))
        -- payer / collector
        (D.at [ "topics" ] (D.index 2 ED.address))
        -- amount
        (D.at [ "data" ] ED.bigInt)
        (D.at [ "removed" ] D.bool)


taxDecoder : D.Decoder TaxEvent
taxDecoder =
    taxUbiDecoder TaxEvent


ubiDecoder : D.Decoder UbiEvent
ubiDecoder =
    taxUbiDecoder UbiEvent


colorLogsDecoder : D.Decoder (List (Maybe ColorEvent))
colorLogsDecoder =
    D.list (D.maybe colorDecoder)


priceLogsDecoder : D.Decoder (List (Maybe PriceEvent))
priceLogsDecoder =
    D.list (D.maybe priceDecoder)


transferLogsDecoder : D.Decoder (List (Maybe TransferEvent))
transferLogsDecoder =
    D.list (D.maybe transferDecoder)


taxLogsDecoder : D.Decoder (List (Maybe TaxEvent))
taxLogsDecoder =
    D.list (D.maybe taxDecoder)


ubiLogsDecoder : D.Decoder (List (Maybe UbiEvent))
ubiLogsDecoder =
    D.list (D.maybe ubiDecoder)
