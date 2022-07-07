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
        , int
        , map
        , oneOf
        , run
        , succeed
        , symbol
        , variable
        )
import Set exposing (Set)



-- API


type Param
    = Pixel PixelParam
    | Playback PlaybackParam
    | PlaybackWithCenter PlaybackWithCenterParam


parse : String -> Result (List DeadEnd) Param
parse =
    {-
       `parse` deals with common errors like:
       - blockNumber < 1
       - clamp zoomLevel into valid range
       - clamp not into valid range

       The only runtime check left for user code is
       to see if blockNumber is acutally within the Delta cid pointing
       or one Snapper window afterwards.
    -}
    run param



-- Types


type alias PixelParam =
    Maybe Cell


type alias PlaybackParam =
    { speed : PB.Speed
    , zoom : ZoomLevel
    , blockNumber : BlockNumber
    , rewindCount : Int
    , cid : Cid
    }


type alias PlaybackWithCenterParam =
    { pixel : PixelParam
    , playback : PlaybackParam
    }



-- Parsers and Helpers


param : Parser Param
param =
    oneOf
        [ succeed Pixel |= pixel
        , succeed Playback |= playback
        , succeed PlaybackWithCenter |= playbackWithCenter
        ]


pixel : Parser PixelParam
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
        |. symbol "playback/"
        |= speed
        |. symbol "/"
        |= zoom
        |. symbol "/"
        |= blockNumber
        |. symbol "/"
        |= rewindCount
        |. symbol "/"
        |= cid


playbackWithCenter : Parser PlaybackWithCenterParam
playbackWithCenter =
    succeed PlaybackWithCenterParam
        |= pixel
        |. symbol "/"
        |= playback


speed : Parser PB.Speed
speed =
    oneOf
        [ succeed PB.OneX |. symbol "1X"
        , succeed PB.TwoX |. symbol "2X"
        , succeed PB.FourX |. symbol "4X"
        ]


zoom : Parser ZoomLevel
zoom =
    zoomLevel |. symbol "Z"


zoomLevel : Parser ZoomLevel
zoomLevel =
    let
        validate =
            toFloat >> clamp minZoom maxZoom
    in
    int |> map validate


blockNumber : Parser Int
blockNumber =
    let
        validate =
            min 1
    in
    int |> map validate


rewindCount : Parser Int
rewindCount =
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
    "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
        |> String.toList
        |> Set.fromList
