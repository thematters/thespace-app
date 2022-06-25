module Data exposing (..)

import BigInt exposing (BigInt)
import Config
    exposing
        ( colorHexs
        , highlightZoom
        , mapHeight
        , mapWidth
        , maxInt256
        , maxPrice
        , minPrice
        , rpcProvider
        , shiftedColorHexs
        , zeroPrice
        )
import Contract.Registry exposing (RegistryTransferEvent)
import Contract.TheSpace
    exposing
        ( ColorEvent
        , PriceEvent
        , TaxEvent
        , TransferEvent
        , UbiEvent
        )
import Dict exposing (Dict)
import Eth.Defaults exposing (zeroAddress)
import Eth.Types exposing (Address)
import Eth.Units exposing (EthUnit(..))
import Eth.Utils exposing (add0x, addressToChecksumString)
import Hex
import Json.Decode as D
import Regex
import Time exposing (Posix)



-- Basic


type alias Index =
    Int


type alias ZoomLevel =
    Float


type ZoomDirection
    = In
    | Out


type SortOrder
    = Ascend
    | Descend


type alias Delta =
    Float


type alias Cell =
    { x : Int, y : Int }


type alias Position =
    { x : Float, y : Float }


type alias PositionDelta =
    { dx : Delta, dy : Delta }


type alias Size =
    ( Int, Int )


type alias FloatSize =
    ( Float, Float )


type alias Transform =
    { dx : Delta
    , dy : Delta
    , zoom : ZoomLevel
    }


type alias Pixel =
    { index : Index
    , owner : Address
    , price : Price
    , color : ColorId
    , ubi : Price
    , tax : Price
    , lastTaxBK : BlockNumber
    , ownerBalance : Maybe Price
    , ownerAllowance : Maybe Price
    }


type alias PixelBrief =
    { index : Index
    , price : Price
    , color : ColorId
    }


type alias PixelAsset =
    { cell : Cell
    , color : ColorId
    , price : Price
    }


type alias ColorId =
    Int


type alias ColorHex =
    String


type alias ZIndex =
    Int


type alias Price =
    BigInt


type alias TaxRate =
    Price


type alias BlockNumber =
    Int


type alias HexColor =
    String


type Activity
    = ColorAct ColorEvent
    | PriceAct PriceEvent
    | TransferAct TransferEvent
    | TaxAct TaxEvent
    | UbiAct UbiEvent
    | DefaultAct RegistryTransferEvent
    | TxSendAct Index
    | ActError RpcErrorData


type WalletInfo
    = DetectingWallet
    | NoWallet
    | LockedWallet
    | Wallet WalletDetail


type alias WalletDetail =
    { chainId : Int
    , address : Maybe Address
    , balance : Maybe Price
    , allowance : Maybe Price
    }


type AbbrType
    = AbbrShort
    | AbbrNormal
    | AbbrLong


type alias OwnPixelsResultPage =
    { total : Int
    , limit : Int
    , offset : Int
    , pixels : List Pixel
    }


type TokenInfoKind
    = AllowanceTokenInfo
    | BalanceTokenInfo


type alias TokenInfo =
    { kind : TokenInfoKind
    , address : Address
    , amount : Price
    }


type alias RpcErrorData =
    { kind : RpcErrorKind
    , code : Int
    , message : String
    }


type RpcErrorKind
    = RpcUnderPricedError Index
    | RpcUnknownError



-- Map


inc : Int -> Int
inc i =
    i + 1


dec : Int -> Int
dec i =
    i - 1


sum : List Int -> Int
sum =
    List.foldl (+) 0


roundFloat : Float -> Float
roundFloat x =
    x |> round |> toFloat


floorFloat : Float -> Float
floorFloat x =
    x |> floor |> toFloat


sizeToFloatSize : Size -> FloatSize
sizeToFloatSize ( w, h ) =
    ( w |> toFloat, h |> toFloat )


position : Float -> Float -> Position
position x y =
    { x = x, y = y }


positionDelta : Position -> Position -> PositionDelta
positionDelta p1 p2 =
    { dx = p1.x - p2.x, dy = p1.y - p2.y }


positionDistance : Position -> Position -> Float
positionDistance p1 p2 =
    let
        dx =
            p1.x - p2.x

        dy =
            p1.y - p2.y
    in
    sqrt (dx * dx + dy * dy)


posToCell : Transform -> Position -> Cell
posToCell trans pos =
    { x = truncate ((pos.x - trans.dx) / trans.zoom)
    , y = truncate ((pos.y - trans.dy) / trans.zoom)
    }


cellToPos : Transform -> Cell -> Position
cellToPos trans cell =
    { x = toFloat cell.x * trans.zoom + trans.dx
    , y = toFloat cell.y * trans.zoom + trans.dy
    }


cellToIndex : Cell -> Int
cellToIndex cell =
    cell.y * mapWidth + cell.x |> inc


indexToCell : Int -> Cell
indexToCell idx =
    let
        i =
            dec idx
    in
    { x = modBy mapWidth i
    , y = i // mapWidth
    }


posStringNoParen : Position -> String
posStringNoParen xy =
    (String.fromFloat <| xy.x + 1) ++ "," ++ (String.fromFloat <| xy.y + 1)


cellString : Cell -> String
cellString xy =
    (String.fromInt <| xy.x + 1) ++ "," ++ (String.fromInt <| xy.y + 1)


validIndex : Int -> Bool
validIndex i =
    i >= 1 && i <= (mapWidth * mapHeight)


cellInMap : Cell -> Bool
cellInMap xy =
    xy.x >= 0 && xy.x < mapWidth && xy.y >= 0 && xy.y < mapHeight


cellNotInMap : Cell -> Bool
cellNotInMap =
    cellInMap >> not


lowZoom : ZoomLevel -> Bool
lowZoom z =
    z < highlightZoom


scale : Float -> Float -> Float -> Float
scale from to a =
    if from == 0 then
        0

    else
        (abs to * a / abs from) |> clamp -to to


center : Int -> ZoomLevel -> Int -> Float
center m zoom w =
    let
        center_ : Float -> Int -> Float
        center_ mapDimension winDimension =
            (winDimension - floor mapDimension) // 2 |> toFloat
    in
    center_ (toFloat m * zoom) w


centerCell : Transform -> Size -> Cell
centerCell canvas winSize =
    let
        ( w, h ) =
            winSize
    in
    posToCell canvas { x = toFloat w / 2, y = toFloat h / 2 }


fakePixel : Index -> Pixel
fakePixel i =
    let
        b0 =
            BigInt.fromInt 0
    in
    { index = i
    , owner = zeroAddress
    , price = b0
    , color = 0
    , ubi = b0
    , tax = b0
    , lastTaxBK = 0
    , ownerBalance = Nothing
    , ownerAllowance = Nothing
    }



-- Eth


intToHex : Int -> String
intToHex i =
    i |> Hex.toString |> String.padLeft 64 '0' |> add0x


safePrice : BigInt -> BigInt
safePrice bi =
    if BigInt.compare bi minPrice == LT then
        minPrice

    else if BigInt.compare bi maxPrice == GT then
        maxPrice

    else
        bi


tokenDeposited : Price -> Bool
tokenDeposited alw =
    alw == maxInt256


walletConnected : WalletInfo -> Bool
walletConnected wallet =
    case wallet of
        Wallet { address } ->
            case address of
                Just _ ->
                    True

                _ ->
                    False

        _ ->
            False


walletIsAddress : WalletInfo -> Address -> Bool
walletIsAddress wallet owner =
    case wallet of
        Wallet { address } ->
            if address == Just owner then
                True

            else
                False

        _ ->
            False


taxRateBase : BigInt
taxRateBase =
    BigInt.fromInt <| 1000 * 10000


accTaxNumBlocks_ : Price -> Int -> Int -> TaxRate -> Price
accTaxNumBlocks_ price bk1 bk2 rate =
    let
        bkDelta =
            BigInt.fromInt <| max 0 <| bk2 - bk1

        tax =
            price |> BigInt.mul rate |> BigInt.mul bkDelta
    in
    BigInt.div tax taxRateBase


accTaxNumBlocks : Price -> Int -> Maybe TaxRate -> Price
accTaxNumBlocks price bkN taxRate =
    Maybe.map (accTaxNumBlocks_ price 0 bkN) taxRate
        |> Maybe.withDefault zeroPrice


accTax : Address -> Price -> BlockNumber -> Maybe BlockNumber -> Maybe TaxRate -> Price
accTax owner price bk1 bk2 taxRate =
    if owner == zeroAddress then
        zeroPrice

    else
        Maybe.map2 (accTaxNumBlocks_ price bk1) bk2 taxRate
            |> Maybe.withDefault zeroPrice


accUbi : Price -> Size -> Price -> Price
accUbi oldUbi mapSize newTaxAmount =
    let
        ( w, h ) =
            mapSize

        shares =
            BigInt.fromInt <| w * h

        shareAmount =
            BigInt.div
                (BigInt.mul newTaxAmount (BigInt.fromInt 9500))
                (BigInt.fromInt 10000)
    in
    shares |> BigInt.div shareAmount |> BigInt.add oldUbi


abbrString : Int -> String -> String
abbrString len s =
    if String.length s <= len then
        s

    else
        let
            leftLen =
                ((toFloat <| len - 1) / 2) |> ceiling

            rightLen =
                len - 1 - leftLen

            left =
                String.left leftLen s

            right =
                String.right rightLen s
        in
        left ++ "…" ++ right


addressAbbr_ : AbbrType -> Address -> String
addressAbbr_ abbrType address =
    let
        addr =
            addressToChecksumString address

        totalLen =
            case abbrType of
                AbbrShort ->
                    10

                AbbrNormal ->
                    15

                AbbrLong ->
                    20
    in
    abbrString totalLen addr


addressAbbr : Address -> String
addressAbbr =
    addressAbbr_ AbbrShort


priceAbbr : AbbrType -> Price -> String
priceAbbr abbrType price =
    let
        p =
            fromWei Ether price

        decAbbrLen =
            4

        totalLen =
            case abbrType of
                AbbrShort ->
                    10

                AbbrNormal ->
                    16

                AbbrLong ->
                    24
    in
    case p |> String.indexes "." |> List.head of
        Nothing ->
            abbrString totalLen p

        Just i ->
            if String.length p <= totalLen then
                p

            else
                let
                    intLen =
                        i

                    decLen =
                        String.length p - i

                    intStr =
                        String.slice 0 i p

                    decStr =
                        String.slice (i + 1) (String.length p) p

                    rightLen =
                        min decAbbrLen decLen

                    leftLen =
                        totalLen - rightLen - 1 |> min intLen

                    left =
                        abbrString leftLen intStr

                    right =
                        String.left rightLen decStr
                in
                left ++ "." ++ right


priceAbbrShort : Price -> String
priceAbbrShort =
    priceAbbr AbbrShort


priceAbbrNormal : Price -> String
priceAbbrNormal =
    priceAbbr AbbrNormal


priceAbbrLong : Price -> String
priceAbbrLong =
    priceAbbr AbbrLong



--Color


safeColorId : Int -> ColorId
safeColorId i =
    if i < 1 || i > 16 then
        1

    else
        i


colorDict : List ColorHex -> Dict ColorId HexColor
colorDict colors =
    let
        colorIds =
            List.range 1 16
    in
    List.map2 (\a b -> ( a, b )) colorIds colors |> Dict.fromList


hexColorDict : Dict ColorId HexColor
hexColorDict =
    colorDict colorHexs


shiftedHexColorDict : Dict ColorId HexColor
shiftedHexColorDict =
    colorDict shiftedColorHexs


unsafeColorId : Dict ColorId HexColor -> ColorId -> HexColor
unsafeColorId hexDict cid =
    Dict.get cid hexDict |> Maybe.withDefault "000000"


unsafeColorIdToHexColor : ColorId -> HexColor
unsafeColorIdToHexColor =
    unsafeColorId hexColorDict


unsafeColorIdToShiftedHexColor : ColorId -> HexColor
unsafeColorIdToShiftedHexColor =
    unsafeColorId shiftedHexColorDict



-- Rpc


notOnChain : Int -> Bool
notOnChain chainId =
    chainId /= rpcProvider.chainId


cidToSnapshotUri : String -> String
cidToSnapshotUri cid =
    Config.snapshotUriPrefix ++ cid


fromWei : EthUnit -> BigInt -> String
fromWei unit amount =
    -- https://github.com/cmditch/elm-ethereum/blob/5.0.0/src/Eth/Units.elm
    -- fix shift amount to 10 ^ 78
    let
        decimalShift : EthUnit -> Int
        decimalShift unt =
            case unt of
                Wei ->
                    0

                Kwei ->
                    3

                Mwei ->
                    6

                Gwei ->
                    9

                Microether ->
                    12

                Milliether ->
                    15

                Ether ->
                    18

                Kether ->
                    21

                Mether ->
                    24

                Gether ->
                    27

                Tether ->
                    30

        decimalIndex =
            decimalShift unit

        shift =
            78

        amountStr =
            BigInt.toString amount |> String.padLeft shift '0'

        result =
            String.left (shift - decimalIndex) amountStr
                ++ "."
                ++ String.right decimalIndex amountStr
    in
    result
        |> Regex.replace
            (Maybe.withDefault Regex.never
                (Regex.fromString "(^0*(?=0\\.|[1-9]))|(\\.?0*$)")
            )
            (\_ -> "")



-- Debug Stuff


sortActs : List Activity -> List Activity
sortActs actLogs =
    actLogs
        |> List.sortBy
            (\act ->
                case act of
                    ColorAct evt ->
                        evt.blockNumber

                    PriceAct evt ->
                        evt.blockNumber

                    TransferAct evt ->
                        evt.blockNumber

                    TaxAct evt ->
                        evt.blockNumber

                    UbiAct evt ->
                        evt.blockNumber

                    DefaultAct evt ->
                        evt.blockNumber

                    TxSendAct idx ->
                        idx

                    ActError _ ->
                        0
            )
        |> List.reverse
