module View.Canvas exposing
    ( viewCanvas
    , viewPointingHighlight
    , viewQueuedHighlights
    , viewSelectHighlight
    )

import Config
    exposing
        ( canvaszIndex
        , pointingCellzIndex
        , queuedCellzIndex
        , selectCellzIndex
        )
import Css exposing (..)
import Data
    exposing
        ( Cell
        , ColorHex
        , ColorId
        , Index
        , Position
        , Transform
        , ZIndex
        , cellNotInMap
        , cellToPos
        , floorFloat
        , indexToCell
        , lowZoom
        , unsafeColorIdToShiftedHexColor
        )
import Dict exposing (Dict)
import Html.Events.Extra.Mouse exposing (onDown)
import Html.Events.Extra.Wheel exposing (onWheel)
import Html.Styled exposing (Html, canvas, div)
import Html.Styled.Attributes exposing (css, fromUnstyled, id)
import Model exposing (Dragging(..), SelectCell(..))
import Msg exposing (Msg(..))
import View.Common as C


viewCanvas : Dragging -> Position -> Html Msg
viewCanvas dragging pagePos =
    let
        cursor_ =
            case dragging of
                NotDragging ->
                    default

                MiniMapDragging _ ->
                    move

                MapDragging pos ->
                    if pos == pagePos then
                        default

                    else
                        grab
    in
    canvas
        [ id "canvas"
        , css [ zIndex (int canvaszIndex), cursor cursor_ ]
        , onDown C.mapMouseDownHandler |> fromUnstyled
        , onWheel C.mouseWheelHandler |> fromUnstyled
        ]
        []


viewPointingHighlight : Dragging -> Transform -> Cell -> Html msg
viewPointingHighlight dragging cvs cell =
    if dragging /= NotDragging || lowZoom cvs.zoom || cellNotInMap cell then
        C.phantomDiv

    else
        highlightCell cvs pointingCellzIndex C.highlightColor1Str cell


viewSelectHighlight : Transform -> SelectCell -> Html msg
viewSelectHighlight cvs cell_ =
    if lowZoom cvs.zoom then
        C.phantomDiv

    else
        let
            highlight idx =
                highlightCellAtIndex cvs selectCellzIndex C.highlightColor2Str idx
        in
        case cell_ of
            NoCell ->
                C.phantomDiv

            LoadingCell index ->
                highlight index

            LoadedCell ( { index }, _, _ ) ->
                highlight index


viewQueuedHighlights : Dict Index ColorId -> Transform -> Html msg
viewQueuedHighlights queue cvs =
    if lowZoom cvs.zoom then
        C.phantomDiv

    else
        let
            highlight ( idx, cid ) =
                let
                    colorHex =
                        unsafeColorIdToShiftedHexColor cid
                in
                highlightCellAtIndex cvs queuedCellzIndex colorHex idx
        in
        div [] <| List.map highlight <| Dict.toList <| queue


highlightCell : Transform -> ZIndex -> ColorHex -> Cell -> Html msg
highlightCell cvs zIndex colorHex cell =
    let
        pos =
            cellToPos cvs cell

        ( x, y ) =
            ( floorFloat pos.x, floorFloat pos.y )
    in
    C.highlight x y cvs.zoom cvs.zoom 2 colorHex zIndex


highlightCellAtIndex : Transform -> ZIndex -> ColorHex -> Index -> Html msg
highlightCellAtIndex cvs zIndex colorHex index =
    indexToCell index |> highlightCell cvs zIndex colorHex
