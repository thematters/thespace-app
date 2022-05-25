module Config exposing (..)

import BigInt exposing (BigInt)
import Env exposing (env)
import Eth.Types exposing (Address)
import Eth.Utils exposing (addressToString, unsafeToHex)


type alias HexColor =
    String


colorHexs : List HexColor
colorHexs =
    [ "000000"
    , "ffffff"
    , "d4d7d9"
    , "898d90"
    , "784102"
    , "d26500"
    , "ff8a00"
    , "ffde2f"
    , "159800"
    , "8de763"
    , "58eaf4"
    , "059df2"
    , "034cba"
    , "9503c9"
    , "d90041"
    , "ff9fab"
    ]


shiftedColorHexs : List HexColor
shiftedColorHexs =
    [ "2e2e2e"
    , "efefef"
    , "c6c6c7"
    , "777778"
    , "603401"
    , "ae5401"
    , "ea7e00"
    , "f3cc00"
    , "147404"
    , "55d917"
    , "32e1ee"
    , "017fc7"
    , "023d94"
    , "700197"
    , "b90037"
    , "ff7b8b"
    ]


lightColor : HexColor -> Bool
lightColor c =
    c == "ffffff"


mapWidth : Int
mapWidth =
    env.mapWidth


mapHeight : Int
mapHeight =
    env.mapHeight


mapSize : ( Int, Int )
mapSize =
    ( mapWidth, mapHeight )


minZoom : Float
minZoom =
    env.minZoom


maxZoom : Float
maxZoom =
    38


highlightZoom : Float
highlightZoom =
    16 |> clamp minZoom maxZoom


clickZoom : Float
clickZoom =
    24 |> clamp minZoom maxZoom


sidebarWidth : Float
sidebarWidth =
    330


miniMapSide : Float
miniMapSide =
    300


miniMapWidth : Float
miniMapWidth =
    -- plus 2 for better effect under blur(1px)
    miniMapSide + 2


miniMapHeight : Float
miniMapHeight =
    -- plus 2 for better effect under blur(1px)
    miniMapSide + 2


cellModalWidth : Float
cellModalWidth =
    446


cellModalHeight : Float
cellModalHeight =
    446


canvaszIndex : Int
canvaszIndex =
    1


pointingCellzIndex : Int
pointingCellzIndex =
    97


selectCellzIndex : Int
selectCellzIndex =
    98


queuedCellzIndex : Int
queuedCellzIndex =
    99


sidebarzIndex : Int
sidebarzIndex =
    100


miniMapzIndex : Int
miniMapzIndex =
    sidebarzIndex


cellModalzIndex : Int
cellModalzIndex =
    sidebarzIndex + 1


notifzIndex : Int
notifzIndex =
    cellModalzIndex + 1


moveClampRatio : Float
moveClampRatio =
    1 / 2


playbackTicks : Int
playbackTicks =
    240


maxManagableAssets : Int
maxManagableAssets =
    1000


minInt256 : BigInt
minInt256 =
    BigInt.fromInt 0


maxInt256 : BigInt
maxInt256 =
    let
        b1 =
            BigInt.fromInt 1

        b2 =
            BigInt.fromInt 2

        b256 =
            BigInt.fromInt 256
    in
    BigInt.sub (BigInt.pow b2 b256) b1


maxPrice256 : BigInt
maxPrice256 =
    -- The largest round Ether 256 bits can hold in Wei:
    -- 115792089237316195423570985008687907853269984665640564039457
    let
        b2 =
            BigInt.fromInt 2

        bG =
            BigInt.fromInt 1000000000

        factor =
            BigInt.pow bG b2
    in
    BigInt.div maxInt256 factor |> BigInt.mul factor


maxPrice : BigInt
maxPrice =
    -- 1 billion Eth
    let
        b10 =
            BigInt.fromInt 10

        bG =
            BigInt.fromInt 1000000000
    in
    BigInt.mul bG bG |> BigInt.mul b10


zeroPrice : BigInt
zeroPrice =
    minInt256


minPrice : BigInt
minPrice =
    -- 0 Wei
    zeroPrice


getOwnPixelPage : Int
getOwnPixelPage =
    if debug then
        5

    else
        100


getOwnPixelLimit : Int
getOwnPixelLimit =
    (if debug then
        101

     else
        10000
    )
        |> max getOwnPixelPage



-- Topcis


color =
    "8da7074ffa2c919782faaf9705c7edfe7f814551a91b91aed83ee2ef5ac6af27"


price =
    "75a0543aefc16d03b25751bdf0b5a2fbbec05c6436fd60b038d40f5b7d1def83"


transfer =
    "c12a40285e912bef139dd458d6bb29f54ba3b7bb9513d442a8c16a3f0a649eed"


tax =
    "c5790222911f43ca7d78c4f5ef5cb5a21d7fda4d923d433b80e7db9c295de88a"


ubi =
    "a760fe80056c46d089c37a35d9dbe762141a463ae0eb8235522d27ab9595286d"


snapshot =
    "33038a04092464e1b8ce6006a75d1518a1226c32d269977e141f566ea7e284ab"


topics =
    { color = color |> unsafeToHex
    , price = price |> unsafeToHex
    , transfer = transfer |> unsafeToHex
    , tax = tax |> unsafeToHex
    , ubi = ubi |> unsafeToHex
    , snapshot = snapshot |> unsafeToHex
    }


rpcSocketAddress =
    env.rpcSocketAddress


rpcProvider =
    env.rpcProvider


contracts =
    env.contracts


debug =
    env.debug


tokenSign : String
tokenSign =
    "$SPACE"



-- Links


aboutLink : String
aboutLink =
    "https://thespace.game/"


installMetaMaskLink : String
installMetaMaskLink =
    "https://metamask.io/"


getTokenLink : String
getTokenLink =
    "https://forms.gle/NLmwXdta156JPnfq5"


whatAreTheseNumbersLink : String
whatAreTheseNumbersLink =
    "https://wiki.thespace.game/introduction-to-the-space#0807e8a7cf96446ebfd91ed7eef29805"


underPricedHelpLink : String
underPricedHelpLink =
    "https://thespace.game/"


snapshotUriPrefix : String
snapshotUriPrefix =
    env.snapshotUriPrefix


polyscanEndpoint : String
polyscanEndpoint =
    env.polyscanEndpoint


toPolyscanAddressUrl : Address -> String
toPolyscanAddressUrl address =
    polyscanEndpoint ++ "address/" ++ addressToString address


toPolyscanTokenUrl : Int -> String
toPolyscanTokenUrl tokenId =
    let
        tokenIdStr =
            String.fromInt tokenId

        registryStr =
            addressToString contracts.registry
    in
    polyscanEndpoint ++ "token/" ++ registryStr ++ "?a=" ++ tokenIdStr
