module View exposing (view)

import Html.Styled exposing (Html, div)
import Html.Styled.Lazy exposing (lazy2, lazy3, lazy5, lazy6, lazy7)
import Model exposing (AppMode(..), Model)
import Msg exposing (Msg)
import View.Canvas as Canvas
import View.Cell as Cell
import View.Common exposing (globalStyle)
import View.MiniMap as MiniMap
import View.Notif as Notif
import View.Sidebar as Sidebar
import View.Sponsor as Sponsor


view : Model -> Html Msg
view m =
    let
        canvas =
            lazy2 Canvas.viewCanvas m.dragging m.pagePos

        minimap =
            lazy5 MiniMap.viewMiniMap m.miniMapMode m.dragging m.winSize m.canvas m.cellPos

        basicViews =
            globalStyle m.mode
                ++ [ lazy2 Notif.viewNotif m.winSize m.notif
                   , lazy3 Canvas.viewPointingHighlight m.dragging m.canvas m.cellPos
                   , Sponsor.viewSponsor
                   ]

        infoViews =
            [ lazy7 Sidebar.viewSidebar m.sidebarMode m.winSize m.wallet m.acts m.assets m.sidebarInfLists m.taxInfo
            , lazy2 Canvas.viewSelectHighlight m.canvas m.selectCell
            , lazy2 Canvas.viewQueuedHighlights m.queue m.canvas
            , lazy7 Cell.viewSelectCell m.cellModalMode m.winSize m.canvas m.selectCell m.wallet m.taxInfo m.input
            ]

        realtimeViews =
            basicViews ++ infoViews

        playbackViews =
            basicViews

        views =
            case m.mode of
                RealtimeLoading ->
                    realtimeViews

                Realtime ->
                    realtimeViews

                PlaybackLoading ->
                    playbackViews

                Playback _ ->
                    playbackViews
    in
    -- We use this :: trick to make sure two canvases can be captured by Js
    -- See #19 in: https://github.com/elm/html/issues/53
    div [] (minimap :: (canvas :: views))
