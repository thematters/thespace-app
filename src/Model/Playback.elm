module Model.Playback exposing (..)

import Data exposing (BlockNumber)


type alias PlaybackConfig =
    { from : Int
    , to : Int
    , shareFrom : Int
    , shareTo : Int
    , current : Int
    , status : PlaybackStatus
    , speed : PlaybackSpeed
    }


type PlaybackStatus
    = PlaybackStarted
    | PlaybackPaused


type PlaybackSpeed
    = OneX
    | TwoX
    | FourX


initPlaybackConfig : BlockNumber -> BlockNumber -> PlaybackConfig
initPlaybackConfig from to =
    { from = from
    , to = to
    , shareFrom = from
    , shareTo = to
    , speed = OneX
    , current = 0
    , status = PlaybackPaused
    }


playbackSpeedToString : PlaybackSpeed -> String
playbackSpeedToString spd =
    case spd of
        OneX ->
            "1X"

        TwoX ->
            "2X"

        FourX ->
            "4X"


nextPlaybackSpeed : PlaybackSpeed -> PlaybackSpeed
nextPlaybackSpeed spd =
    case spd of
        OneX ->
            TwoX

        TwoX ->
            FourX

        FourX ->
            OneX
