port module Canvas exposing
    ( canvasSubs
    , centerToCellTransfrom
    , endPlayback
    , forward
    , initLatestColors
    , initMapSnapshot
    , initPlayback
    , move
    , moveClampToEdge
    , moveTransfrom
    , playbackChangeSpeed
    , playbackSkipToEnd
    , playbackSkipToStart
    , redrawMiniMap
    , reset
    , resetTransform
    , rewind
    , scale
    , startPlayback
    , startPlaybackAgain
    , update
    , zoomTransform
    )

import Array exposing (Array)
import Config
    exposing
        ( cellModalHeight
        , clickZoom
        , colorHexs
        , mapSize
        , minZoom
        , moveClampRatio
        )
import Data
    exposing
        ( Cell
        , ColorChange
        , ColorEvent
        , Delta
        , Index
        , Size
        , Transform
        , ZoomLevel
        , center
        , dec
        , indexToCell
        , roundFloat
        , safeColorId
        , sizeToFloatSize
        , validIndex
        )
import Model exposing (AppMode(..), Model)
import Model.Playback exposing (PlaybackSpeed(..), initPlaybackConfig)
import Msg exposing (Msg(..))



-- Ports


port canvasOut : String -> Cmd msg


port canvasIn : (String -> msg) -> Sub msg



-- Math


resetTransform : Size -> Transform
resetTransform winSize =
    let
        ( mW, mH ) =
            mapSize

        ( wW, wH ) =
            winSize
    in
    { dx = center mW minZoom wW, dy = center mH minZoom wH, zoom = minZoom }


moveClampToEdge : ZoomLevel -> Size -> ( Float, Float ) -> ( Float, Float )
moveClampToEdge zoom winSize trans =
    let
        ( wW, wH ) =
            winSize |> sizeToFloatSize

        ( mW, mH ) =
            mapSize |> sizeToFloatSize

        rmW =
            zoom * mW

        rmH =
            zoom * mH

        ( dx, dy ) =
            trans
    in
    ( dx |> clamp (min 0 (wW - rmW)) (max 0 (wW - rmW)) |> roundFloat
    , dy |> clamp (min 0 (wH - rmH)) (max 0 (wH - rmH)) |> roundFloat
    )


moveClamp : ZoomLevel -> Size -> ( Float, Float ) -> ( Float, Float )
moveClamp zoom winSize trans =
    let
        ( wW, wH ) =
            winSize |> sizeToFloatSize

        ( mW, mH ) =
            mapSize |> sizeToFloatSize

        dW =
            wW * moveClampRatio

        dH =
            wH * moveClampRatio

        ( dx, dy ) =
            trans
    in
    ( dx |> clamp (dW - zoom * mW) (wW - dW) |> roundFloat
    , dy |> clamp (dH - zoom * mH) (wH - dH) |> roundFloat
    )


moveTransfrom : Delta -> Delta -> Size -> Transform -> Transform
moveTransfrom dx dy winSize cvs =
    let
        ( dx1, dy1 ) =
            ( cvs.dx + dx, cvs.dy + dy ) |> moveClamp cvs.zoom winSize
    in
    { dx = dx1, dy = dy1, zoom = cvs.zoom }


zoomTransform : Size -> Cell -> ZoomLevel -> Transform -> Transform
zoomTransform winSize cell zoom canvas =
    let
        ( dx, dy ) =
            ( canvas.dx + (canvas.zoom - zoom) * toFloat cell.x
            , canvas.dy + (canvas.zoom - zoom) * toFloat cell.y
            )
                |> moveClamp zoom winSize
    in
    { dx = dx, dy = dy, zoom = zoom }


centerToCellTransfrom : Size -> ZoomLevel -> Index -> Transform
centerToCellTransfrom winSize zoom_ index =
    let
        cell =
            indexToCell index

        ( wW, wH ) =
            winSize

        zoom =
            zoom_ |> max clickZoom

        dx =
            toFloat wW / 2 - zoom * toFloat cell.x

        dy =
            toFloat wH / 2 - cellModalHeight / 2 - zoom * toFloat cell.y
    in
    { dx = dx |> roundFloat, dy = dy |> roundFloat, zoom = zoom }



-- Helpers


send : String -> Cmd msg
send =
    canvasOut



-- API


initMapSnapshotCommand : Transform -> Size -> String -> String
initMapSnapshotCommand trans winSize initImageUri =
    let
        ( mW, mH ) =
            mapSize

        ( wW, wH ) =
            winSize
    in
    "initMapSnapshot,"
        ++ String.fromFloat trans.dx
        ++ ","
        ++ String.fromFloat trans.dy
        ++ ","
        ++ String.fromFloat trans.zoom
        ++ ","
        ++ String.fromInt wW
        ++ ","
        ++ String.fromInt wH
        ++ ","
        ++ String.fromInt mW
        ++ ","
        ++ String.fromInt mH
        ++ ","
        ++ String.join "," colorHexs
        ++ ","
        ++ initImageUri


initMapSnapshot : Transform -> Size -> String -> Cmd msg
initMapSnapshot trans winSize initImageUri =
    -- init,<dx>,<dy>,<zoom>,<winW>,<winH>,<mapW>,<mapH><16colorcodes>,<uri>
    initMapSnapshotCommand trans winSize initImageUri |> send


toIdxCC : Maybe ColorEvent -> String
toIdxCC cevt =
    case cevt of
        Nothing ->
            ""

        Just { removed, index, color } ->
            if not <| validIndex index || removed then
                ""

            else
                String.fromInt (dec index)
                    ++ ","
                    ++ (String.fromInt <| dec <| safeColorId color)


coloEventToCmdStr : List (Maybe ColorEvent) -> String
coloEventToCmdStr cevts =
    cevts
        |> List.map toIdxCC
        |> List.filter (String.isEmpty >> not)
        |> String.join ","


initLatestColors : List (Maybe ColorEvent) -> Cmd msg
initLatestColors cevts =
    -- initLatestColors,<i1>,<cc1>,<i2>,<cc2>...
    let
        idxCCStr =
            coloEventToCmdStr cevts
    in
    if idxCCStr == "" then
        -- we still need to send this to finish initialization when no colors
        "initLatestColors" |> send

    else
        "initLatestColors," ++ coloEventToCmdStr cevts |> send


update : List (Maybe ColorEvent) -> Cmd msg
update cevts =
    -- update,<i1>,<cc1>,<i2>,<cc2>...
    let
        idxCCStr =
            coloEventToCmdStr cevts
    in
    if idxCCStr == "" then
        Cmd.none

    else
        "update," ++ idxCCStr |> send


moveCommand : Transform -> String
moveCommand trans =
    String.fromFloat trans.dx ++ "," ++ String.fromFloat trans.dy


move : Transform -> Cmd msg
move trans =
    -- trans,<dx>,<dy>
    "move," ++ moveCommand trans |> send


scaleCommand : Transform -> String
scaleCommand trans =
    "scale," ++ moveCommand trans ++ "," ++ String.fromFloat trans.zoom


scale : Transform -> Cmd msg
scale trans =
    -- scale,<dx>,<dy>,<zoom>
    scaleCommand trans |> send


resetCommand : Transform -> Size -> String
resetCommand trans size =
    let
        ( w, h ) =
            size
    in
    "reset,"
        ++ moveCommand trans
        ++ ","
        ++ String.fromFloat trans.zoom
        ++ ","
        ++ String.fromInt w
        ++ ","
        ++ String.fromInt h


reset : Transform -> Size -> Cmd msg
reset trans size =
    -- reset,<dx>,<dy>,<zoom>,<winW>,<winH>
    resetCommand trans size |> send


redrawMiniMap : Cmd msg
redrawMiniMap =
    "redrawmm" |> send


initPlayback : Cmd msg
initPlayback =
    "pbInit" |> send


startPlayback : Cmd msg
startPlayback =
    "pbStart" |> send


startPlaybackAgain : Cmd msg
startPlaybackAgain =
    "pbStartAgain" |> send


forwardCommand : Array ColorChange -> String
forwardCommand colorChanges =
    let
        one change =
            String.fromInt change.idx ++ "," ++ change.new
    in
    "pbForward," ++ String.join "," (Array.toList <| Array.map one colorChanges)


forward : Array ColorChange -> Cmd msg
forward colorChanges =
    -- pbForward,<i1>,<cc1>,<i2>,<cc2>...
    forwardCommand colorChanges |> send


rewindCommand : Array ColorChange -> String
rewindCommand colorChanges =
    let
        one change =
            String.fromInt change.idx ++ "," ++ change.old
    in
    "pbRewind," ++ String.join "," (Array.toList <| Array.map one colorChanges)


rewind : Array ColorChange -> Cmd msg
rewind colorChanges =
    -- pbRewind,<i1>,<cc1>,<i2>,<cc2>...
    rewindCommand colorChanges |> send


endPlayback : Cmd msg
endPlayback =
    "pbEnd" |> send


playbackSkipToStart : Cmd msg
playbackSkipToStart =
    "pbSkipToStart" |> send


playbackSkipToEnd : Cmd msg
playbackSkipToEnd =
    "pbSkipToEnd" |> send


changeSpeedCommand : Int -> String
changeSpeedCommand speed =
    "pbSpeed," ++ String.fromInt speed


playbackChangeSpeed : PlaybackSpeed -> Cmd msg
playbackChangeSpeed spd =
    -- pbSpeed,<spd>
    case spd of
        OneX ->
            changeSpeedCommand 1 |> send

        TwoX ->
            changeSpeedCommand 2 |> send

        FourX ->
            changeSpeedCommand 4 |> send



-- Subscriptions


canvasSubs : Model -> List (Sub Msg)
canvasSubs model =
    [ canvasIn <| handleAckMessages model ]


handleAckMessages : Model -> (String -> Msg)
handleAckMessages model =
    let
        handler msg =
            case msg of
                "initedMapSnapshot" ->
                    MapSnapshotInited

                "inited" ->
                    AppModeChange Realtime

                "pbInited" ->
                    case model.mode of
                        PlaybackLoading ->
                            let
                                from =
                                    1

                                to =
                                    Array.length model.colorHistory

                                initConfig =
                                    initPlaybackConfig from to
                            in
                            AppModeChange <| Playback initConfig

                        RealtimeLoading ->
                            NoOp

                        Realtime ->
                            NoOp

                        Playback _ ->
                            NoOp

                "tick" ->
                    PlaybackTick

                _ ->
                    NoOp
    in
    handler
