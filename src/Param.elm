module Param exposing (Param(..), parse)

import Config exposing (maxRewindEvents, maxZoom, minRewindEvents, minZoom)
import Contract.Snapper exposing (Cid)
import Data exposing (BlockNumber, Cell, ZoomLevel, cellInMap)
import Model.Playback as PB
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , end
        , int
        , map
        , oneOf
        , run
        , spaces
        , succeed
        , symbol
        , variable
        )
import Set exposing (Set)



-- API


type Param
    = JumptoPixel JumpToPixelParam
    | Playback PlaybackParam


parse : String -> Result (List DeadEnd) Param
parse =
    {-
       `parse` deals with these errors:
           - force speed to valid value
           - clamp zoomLevel into valid range
           - force end >= 1
           - clamp length into valid range

       The only runtime check left for user code is
       to see if `end` is acutally within the Delta `cid` pointing at
       or one Snapper window afterwards.
    -}
    run param



-- Types


type alias JumpToPixelParam =
    { to : Maybe Cell }


type alias PlaybackParam =
    { speed : PB.Speed
    , zoom : ZoomLevel
    , center : Maybe Cell
    , end : BlockNumber -- playback end at this block
    , length : Int -- playback this many color changes
    , cid : Cid
    }



-- Parsers and Helpers


param : Parser Param
param =
    oneOf
        [ succeed JumptoPixel |= jumpToPixel
        , succeed Playback |= playback
        ]


jumpToPixel : Parser JumpToPixelParam
jumpToPixel =
    succeed JumpToPixelParam
        |. symbol "@"
        |= pixel
        |. spaces
        |. end


pixel : Parser (Maybe Cell)
pixel =
    let
        validate c =
            if cellInMap c then
                Just c

            else
                Nothing
    in
    cell |> map validate


cell : Parser Cell
cell =
    succeed Cell
        |= int
        |. symbol ","
        |= int


playback : Parser PlaybackParam
playback =
    succeed PlaybackParam
        |. symbol "#playback/"
        |= speed
        |. symbol "/"
        |= zoom
        |. symbol "/"
        |= pixel
        |. symbol "/"
        |= block
        |. symbol "/"
        |= length
        |. symbol "/"
        |= cid


speed : Parser PB.Speed
speed =
    let
        validate i =
            case i of
                2 ->
                    PB.TwoX

                4 ->
                    PB.FourX

                _ ->
                    PB.OneX
    in
    int |. symbol "X" |> map validate


zoom : Parser ZoomLevel
zoom =
    let
        validate =
            toFloat >> clamp minZoom maxZoom
    in
    int |. symbol "Z" |> map validate


block : Parser Int
block =
    let
        validate =
            max 1
    in
    int |> map validate


length : Parser Int
length =
    let
        validate =
            clamp minRewindEvents maxRewindEvents
    in
    int |> map validate


cid : Parser Cid
cid =
    variable
        { start = isBase58Char
        , inner = isBase58Char
        , reserved = Set.empty
        }


isBase58Char : Char -> Bool
isBase58Char c =
    Set.member c base58Alphabet


base58Alphabet : Set Char
base58Alphabet =
    base58CharsStr |> String.toList |> Set.fromList


base58CharsStr : String
base58CharsStr =
    "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
