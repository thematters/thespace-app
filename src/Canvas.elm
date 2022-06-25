port module Canvas exposing
    ( canvasSubs
    , centerToCellTransform
    , endPlayback
    , enterPlayback
    , forward
    , freeMoveTransform
    , initLatestColors
    , initMapSnapshot
    , initPlayback
    , moveClampToEdgeTransform
    , moveTransform
    , playAgain
    , playbackChangeSpeed
    , playbackTimelineBackwards
    , redrawMiniMap
    , reset
    , resetTransform
    , rewind
    , transform
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
import Dict
import Model exposing (AppMode(..), Model)
import Model.Playback as PB
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


moveClampToEdgeTransform : ZoomLevel -> Size -> ( Float, Float ) -> ( Float, Float )
moveClampToEdgeTransform zoom winSize trans =
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


moveTransform : Delta -> Delta -> Size -> Transform -> Transform
moveTransform dx dy winSize cvs =
    let
        ( dx1, dy1 ) =
            ( cvs.dx + dx, cvs.dy + dy ) |> moveClamp cvs.zoom winSize
    in
    { dx = dx1, dy = dy1, zoom = cvs.zoom }


freeMoveTransform : Delta -> Delta -> Transform -> Transform
freeMoveTransform dx dy cvs =
    -- move transfrom but no clamping to 1/2 window width/height
    let
        ( dx1, dy1 ) =
            ( cvs.dx + dx, cvs.dy + dy )
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


centerToCellTransform : Size -> ZoomLevel -> Index -> Transform
centerToCellTransform winSize zoom_ index =
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


toIdxCC : ColorEvent -> String
toIdxCC { removed, index, color } =
    if not <| validIndex index || removed then
        ""

    else
        String.fromInt (dec index)
            ++ ","
            ++ (String.fromInt <| dec <| safeColorId color)


coloEventToCmdStr : List ColorEvent -> String
coloEventToCmdStr cevts =
    cevts
        |> List.map toIdxCC
        |> List.filter (String.isEmpty >> not)
        |> String.join ","


initLatestColors : List ColorEvent -> Cmd msg
initLatestColors cevts =
    -- initLatestColors,<i1>,<cc1>,<i2>,<cc2>...
    let
        idxCCStr =
            coloEventToCmdStr cevts
    in
    if idxCCStr == "" then
        "initLatestColors" |> send

    else
        "initLatestColors," ++ coloEventToCmdStr cevts |> send


update : List ColorEvent -> Cmd msg
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


transCommand : Transform -> String
transCommand trans =
    "trans,"
        ++ String.fromFloat trans.dx
        ++ ","
        ++ String.fromFloat trans.dy
        ++ ","
        ++ String.fromFloat trans.zoom


transform : Transform -> Cmd msg
transform trans =
    -- trans,<dx>,<dy>,<zoom>
    transCommand trans |> send


resetCommand : Transform -> Size -> String
resetCommand trans size =
    let
        ( w, h ) =
            size
    in
    transCommand trans
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


initPlayback : String -> Cmd msg
initPlayback playbackSnapshotUri =
    "pbInit," ++ playbackSnapshotUri |> send


enterPlayback : Cmd msg
enterPlayback =
    "pbStart" |> send


playAgain : Cmd msg
playAgain =
    "pbPlayAgain" |> send


ccToIdxCid : PB.ColorChangeCompatible compatible -> String
ccToIdxCid change =
    let
        idx =
            String.fromInt <| dec change.index

        cId =
            String.fromInt <| dec <| safeColorId change.color
    in
    idx ++ "," ++ cId


timelineToIdxCidString : Array (PB.ColorChangeCompatible compatible) -> String
timelineToIdxCidString timeline =
    timeline |> Array.map ccToIdxCid |> Array.toList |> String.join ","


playbackTimelineBackwards : PB.Timeline -> Cmd msg
playbackTimelineBackwards timeline =
    -- pbReverse,<i1>,<cc1>,<i2>,<cc2>...
    "pbReverse," ++ timelineToIdxCidString timeline |> send


forward : PB.Timeline -> Cmd msg
forward timeline =
    -- pbForward,<i1>,<cc1>,<i2>,<cc2>...
    "pbForward," ++ timelineToIdxCidString timeline |> send


rewind : PB.TimelineBackwards -> Cmd msg
rewind timeline =
    -- pbRewind,<i1>,<cc1>,<i2>,<cc2>...
    "pbRewind," ++ timelineToIdxCidString timeline |> send


endPlayback : Cmd msg
endPlayback =
    "pbEnd" |> send


changeSpeedCommand : Int -> String
changeSpeedCommand speed =
    "pbSpeed," ++ String.fromInt speed


playbackChangeSpeed : PB.Speed -> Cmd msg
playbackChangeSpeed spd =
    -- pbSpeed,<spd>
    case spd of
        PB.OneX ->
            changeSpeedCommand 1 |> send

        PB.TwoX ->
            changeSpeedCommand 2 |> send

        PB.FourX ->
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
                    PlaybackSnapshotReady

                "pbStarted" ->
                    case model.mode of
                        Realtime ->
                            AppModeChange Playback

                        _ ->
                            NoOp

                "tick" ->
                    PlaybackTicked

                s ->
                    PlaybackTimelineBackwards <| String.split "," s
    in
    handler
