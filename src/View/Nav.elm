module View.Nav exposing (viewNav)

import Css exposing (..)
import Data
    exposing
        ( AppMode(..)
        , PlaybackConfig
        , PlaybackStatus(..)
        , playbackSpeedToString
        )
import Data.Image exposing (logoDataUri)
import Html.Styled exposing (Html, div, img, input, span, text)
import Html.Styled.Attributes exposing (css, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Material.Icons.Outlined
    exposing
        ( content_cut
        , highlight_off
        , history
        , pause_circle
        , play_circle
        , share
        , skip_next
        , skip_previous
        )
import Msg exposing (Msg(..))
import View.Common exposing (..)


viewNav : AppMode -> Html Msg
viewNav appMode =
    case appMode of
        RealtimeLoading ->
            viewLogo

        Realtime ->
            viewRealtime

        PlaybackLoading ->
            viewPlaybackLoading

        Playback config ->
            viewPlayback config


basicStyle =
    [ position absolute
    , zIndex (int 100)
    , top (px modalEdge)
    , right (px modalEdge)
    , height (px 48)
    , padding (px 20)
    , modalRadius
    , modalBoxShadow
    , displayFlex
    , alignItems center
    , property "gap" "10px"
    ]


normalStyle =
    backgroundColor modalBackgroundColor :: basicStyle


invertStyle =
    backgroundColor modalBackgroundColorInvert :: basicStyle


iconStyle =
    [ cursor pointer ]


viewLogo : Html Msg
viewLogo =
    div
        [ css normalStyle ]
        [ img
            [ src logoDataUri
            , css [ height (px 48), marginRight (px 10) ]
            ]
            []
        ]


viewRealtime : Html Msg
viewRealtime =
    let
        icon =
            iconNormal

        logo =
            img
                [ src logoDataUri
                , css [ height (px 48), marginRight (px 10) ]
                ]
                []

        playbackIcon =
            div
                [ css iconStyle
                , onClick <| AppModeChange <| PlaybackLoading
                ]
                [ icon history False 42 ]
    in
    div
        [ css normalStyle ]
        [ logo

        --, playbackIcon
        ]


viewPlaybackLoading : Html Msg
viewPlaybackLoading =
    let
        icon =
            iconInvert
    in
    div
        [ css invertStyle ]
        [ img
            [ src logoDataUri
            , css
                [ height (px 48)
                , marginRight (px 10)
                , property "filter" "invert(1)"
                ]
            ]
            []
        , spinner 36 grayStr
        , span [ css [ color (hex "eee") ] ] [ text "Loading playback" ]
        , div
            [ css iconStyle, onClick <| AppModeChange <| Realtime ]
            [ icon highlight_off False 42 ]
        ]


viewPlayback : PlaybackConfig -> Html Msg
viewPlayback config =
    let
        icon =
            iconInvert

        progressBarWidth =
            240

        progressPseudoStyle =
            [ width (px 25)
            , height (px 25)
            , border (px 0)
            , backgroundColor highlightColor2
            , cursor pointer
            ]

        progressStyle =
            [ width (px progressBarWidth)
            , height (px 2)
            , opacity (num 0.7)
            , outline none
            , backgroundColor (hex "999")
            , property "-webkit-appearance" "none"
            , property "appearance" "none"
            , property "opacity" "0.7"
            , property "-webkit-transition" ".2s"
            , property "transition" "opacity .2s"
            , pseudoClass "-webkit-slider-thumb"
                (progressPseudoStyle
                    ++ [ property "appearance" "none"
                       , property "-webkit-appearance" "none"
                       ]
                )
            , pseudoClass "-moz-range-thumb" progressPseudoStyle
            ]

        progress from to current =
            input
                [ type_ "range"
                , Html.Styled.Attributes.min <| String.fromInt from
                , Html.Styled.Attributes.max <| String.fromInt to
                , value <| String.fromInt current
                , onInput (\v -> PlaybackSlide v)
                , css progressStyle
                ]
                []
    in
    div
        [ css invertStyle ]
        [ img
            [ src logoDataUri
            , css
                [ height (px 48)
                , marginRight (px 10)
                , property "filter" "invert(1)"
                ]
            ]
            []
        , div
            [ css iconStyle
            , case config.status of
                PlaybackStarted ->
                    onClick PlaybackPause

                PlaybackPaused ->
                    onClick PlaybackStart
            ]
            [ case config.status of
                PlaybackStarted ->
                    icon pause_circle False 42

                PlaybackPaused ->
                    icon play_circle False 42
            ]
        , div
            [ css iconStyle, onClick PlaybackSkipToStart ]
            [ icon skip_previous False 36 ]
        , progress config.from config.to config.current

        --, span [] [ text <| String.fromInt config.current ]
        , div
            [ css iconStyle, onClick PlaybackSkipToEnd ]
            [ icon skip_next False 36 ]
        , span
            [ css
                [ border3 (px 2) solid lightgray
                , borderRadius (px 8)
                , padding (px 4)
                , color lightgray
                , fontWeight bold
                , cursor pointer
                ]
            , onClick PlaybackSpeedChange
            ]
            [ text <| playbackSpeedToString <| config.speed ]
        , div
            [ css <| marginLeft (px 20) :: iconStyle ]
            [ icon content_cut False 32 ]
        , div
            [ css <| marginRight (px 20) :: iconStyle ]
            [ icon share False 32 ]
        , div
            [ css iconStyle, onClick <| AppModeChange <| Realtime ]
            [ icon highlight_off False 42 ]
        ]
