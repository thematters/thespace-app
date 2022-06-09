module View.MiniMap exposing (viewMiniMap)

import Config
    exposing
        ( mapSize
        , maxZoom
        , minZoom
        , miniMapHeight
        , miniMapWidth
        , miniMapzIndex
        )
import Css exposing (..)
import Data
    exposing
        ( Cell
        , Size
        , Transform
        , ZoomLevel
        , cellInMap
        , cellString
        , sizeToFloatSize
        )
import Data.Icon as Icons
import Html.Events.Extra.Mouse exposing (onDown)
import Html.Events.Extra.Touch exposing (onEnd, onMove, onStart)
import Html.Events.Extra.Wheel exposing (onWheel)
import Html.Styled exposing (Html, canvas, div)
import Html.Styled.Attributes exposing (css, fromUnstyled, id)
import Html.Styled.Events exposing (onClick)
import Model exposing (AppMode(..), Dragging(..), MiniMapMode(..))
import Msg exposing (Msg(..))
import View.Common
    exposing
        ( highlight
        , highlightColor2Str
        , iconLight
        , iconNormal
        , miniMapMouseDownHandler
        , miniMapTouchEndHandler
        , miniMapTouchMoveHandler
        , miniMapTouchStartHandler
        , modalBackgroundColor
        , modalBoxShadow
        , modalEdge
        , modalRadius
        , mouseWheelHandler
        , secDiv
        )


vEdge : Float
vEdge =
    14


hEdge : Float
hEdge =
    20


coordsHeight : Float
coordsHeight =
    20


iconHeight : Float
iconHeight =
    24


birdeyeHeight : Float
birdeyeHeight =
    vEdge * 4 + coordsHeight + iconHeight + miniMapHeight


collapsedHeight : Float
collapsedHeight =
    vEdge * 3 + coordsHeight + iconHeight


viewMiniMap : MiniMapMode -> Dragging -> Size -> Transform -> Cell -> Html Msg
viewMiniMap mode dragging winSize canvas cell =
    let
        h =
            case mode of
                BirdeyeMiniMap ->
                    birdeyeHeight

                CollapsedMiniMap ->
                    collapsedHeight

        style_ =
            [ position absolute
            , zIndex (int miniMapzIndex)
            , right (px modalEdge)
            , bottom (px modalEdge)
            , backgroundColor modalBackgroundColor
            , modalRadius
            , modalBoxShadow
            , width (px miniMapWidth)
            , height (px h)
            ]

        containerStyle =
            [ displayFlex
            , flexDirection column
            , justifyContent spaceBetween
            , margin4 (px vEdge) (px 0) (px vEdge) (px 0)
            , height (px <| h - vEdge * 2)
            ]

        opContainerStyle =
            [ displayFlex
            , justifyContent spaceBetween
            , margin4 (px 0) (px hEdge) (px 0) (px hEdge)
            ]
    in
    div
        [ css style_ ]
        [ div [ css containerStyle ]
            [ pointingCell cell
            , miniMapCanvas mode dragging winSize canvas
            , div
                [ css opContainerStyle ]
                [ basicOps mode canvas.zoom, reset mode ]
            ]
        ]


pointingCell : Cell -> Html msg
pointingCell cell =
    let
        style_ =
            [ displayFlex
            , alignItems center
            , property "gap" "4px"
            , margin4 (px 0) (px hEdge) (px 0) (px hEdge)
            , height (px coordsHeight)
            ]

        s =
            if cellInMap cell then
                "Pixel " ++ cellString cell

            else
                ""
    in
    div [ css style_ ] [ secDiv s ]


miniMapCanvas : MiniMapMode -> Dragging -> Size -> Transform -> Html Msg
miniMapCanvas mode dragging winSize cvs =
    let
        containerStyle =
            case mode of
                BirdeyeMiniMap ->
                    [ margin4 (px vEdge) (px 0) (px vEdge) (px 0) ]

                CollapsedMiniMap ->
                    [ display none ]

        cursor_ =
            case dragging of
                MiniMapDragging _ ->
                    move

                _ ->
                    default

        previewStyle =
            -- these -2 +1 math are for better effect under blur(1px)
            [ displayFlex
            , width (px <| miniMapWidth - 2)
            , height (px <| miniMapHeight - 2)
            , margin2 (px 1) (px 1)
            , cursor cursor_
            , property "filter" "blur(1px)"
            ]

        preview =
            canvas
                [ id "minimap"
                , css previewStyle
                , onDown miniMapMouseDownHandler |> fromUnstyled
                , onWheel mouseWheelHandler |> fromUnstyled
                , onStart miniMapTouchStartHandler |> fromUnstyled
                , onMove miniMapTouchMoveHandler |> fromUnstyled
                , onEnd miniMapTouchEndHandler |> fromUnstyled
                ]
                []
    in
    div [ css containerStyle ] [ viewport winSize cvs, preview ]


viewport : Size -> Transform -> Html msg
viewport winSize canvas =
    let
        ( dx, dy, zoom ) =
            ( canvas.dx, canvas.dy, canvas.zoom )

        scaleH =
            Data.scale (mH * zoom) miniMapHeight

        scaleW =
            Data.scale (mW * zoom) miniMapWidth

        ( mW, mH ) =
            mapSize |> sizeToFloatSize

        ( wW, wH ) =
            winSize |> sizeToFloatSize

        x_ =
            if dx >= 0 then
                0

            else
                -dx |> scaleW

        y_ =
            if dy >= 0 then
                0

            else
                -dy |> scaleH

        w_ =
            if dx >= 0 then
                if mW * zoom + dx > wW then
                    -- left out right in
                    (wW - dx) |> scaleW

                else
                    -- both in
                    miniMapWidth

            else if mW * zoom + dx < wW then
                -- left in right out
                (mW * zoom + dx) |> scaleW

            else
                -- both out
                wW |> scaleW

        h_ =
            if dy >= 0 then
                if mH * zoom + dy > wH then
                    -- top out bottom in
                    (wH - dy) |> scaleH

                else
                    -- both in
                    miniMapHeight

            else if mH * zoom + dy < wH then
                -- top in bottom out
                (mH * zoom + dy) |> scaleH

            else
                -- both out
                wH |> scaleH

        ( lineWidth, minSize ) =
            ( 4, 24 )

        ( shiftX, shiftY ) =
            ( 0, (birdeyeHeight - miniMapHeight - lineWidth) / 2 )

        saveMinSave shift fullLength i =
            let
                shifted =
                    i + shift

                subMinSize =
                    fullLength + shift - minSize
            in
            min shifted subMinSize

        clampMinSize =
            max minSize

        ( x, y ) =
            ( x_ |> saveMinSave shiftX miniMapWidth
            , y_ |> saveMinSave shiftY miniMapHeight
            )

        ( w, h ) =
            ( w_ |> clampMinSize, h_ |> clampMinSize )

        ( color, zIndex ) =
            ( highlightColor2Str, miniMapzIndex )
    in
    highlight x y w h lineWidth color zIndex


basicOps : MiniMapMode -> ZoomLevel -> Html Msg
basicOps mode zoom =
    div [ css [ displayFlex, alignItems center, property "gap" "20px" ] ]
        [ switchMode mode, zoomIn zoom, zoomOut zoom ]


switchMode : MiniMapMode -> Html Msg
switchMode mode =
    let
        style_ =
            [ cursor pointer ]

        ( to, icon ) =
            case mode of
                CollapsedMiniMap ->
                    ( BirdeyeMiniMap, Icons.crosshair )

                BirdeyeMiniMap ->
                    ( CollapsedMiniMap, Icons.chevronDown )
    in
    div [ css style_, onClick <| MiniMapModeChange to ] [ iconNormal icon ]


zoomIn : ZoomLevel -> Html Msg
zoomIn =
    zoom_ maxZoom Icons.zoomIn ZoomInCenter


zoomOut : ZoomLevel -> Html Msg
zoomOut =
    zoom_ minZoom Icons.zoomOut ZoomOutCenter


zoom_ : ZoomLevel -> Html msg -> msg -> ZoomLevel -> Html msg
zoom_ limit icon evt zoom =
    if zoom == limit then
        div [] [ iconLight icon ]

    else
        div [ css [ cursor pointer ], onClick evt ] [ iconNormal icon ]


reset : MiniMapMode -> Html Msg
reset mode =
    let
        baseStyle =
            [ cursor pointer ]

        ( style_, icon ) =
            case mode of
                CollapsedMiniMap ->
                    ( marginTop (px -hEdge) :: baseStyle, Icons.maximizeBig )

                BirdeyeMiniMap ->
                    ( baseStyle, Icons.maximize )
    in
    div [ css style_, onClick ZoomReset ] [ iconNormal icon ]
