module Param exposing (Param(..), parse, toString)

import Config exposing (maxRewindEvents, maxZoom, minRewindEvents, minZoom)
import Contract.Snapper exposing (Cid)
import Data exposing (Cell, ZoomLevel, cellString)
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

       runtime check left for user code:
            - if `end` is acutally within the Delta `cid` pointing at
            or one Snapper window afterwards.
            - cell is in map area
    -}
    run param


toString : Param -> String
toString p =
    case p of
        JumptoPixel { to } ->
            cellString to

        Playback pb ->
            "playback/"
                ++ PB.speedToString pb.speed
                ++ "/"
                ++ String.fromFloat pb.zoom
                ++ "/"
                ++ cellString pb.center
                ++ "/"
                ++ pb.cid
                ++ "/"
                ++ String.fromInt pb.endBlockOffset
                ++ "/"
                ++ String.fromInt pb.length



-- Types


type alias JumpToPixelParam =
    { to : Cell }


type alias PlaybackParam =
    { speed : PB.Speed
    , zoom : ZoomLevel
    , center : Cell
    , cid : Cid
    , endBlockOffset : Int
    , length : Int
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
        |= cell
        |. spaces
        |. end


cell : Parser Cell
cell =
    succeed Cell
        |= int
        |. symbol ","
        |= int


playback : Parser PlaybackParam
playback =
    succeed PlaybackParam
        |. symbol "playback/"
        |= speed
        |. symbol "/"
        |= zoom
        |. symbol "/"
        |= cell
        |. symbol "/"
        |= cid
        |. symbol "/"
        |= int
        |. symbol "/"
        |= length


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


zoom : Parser ZoomLevel
zoom =
    let
        validate =
            toFloat >> clamp minZoom maxZoom
    in
    int |. symbol "Z" |> map validate


length : Parser Int
length =
    let
        validate =
            clamp minRewindEvents maxRewindEvents
    in
    int |> map validate
