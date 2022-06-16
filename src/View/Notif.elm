module View.Notif exposing (viewNotif)

import Config exposing (notifzIndex)
import Css exposing (..)
import Css.Animations as Animation exposing (keyframes)
import Data exposing (Size, sizeToFloatSize)
import Data.Icon as Icons
import Html.Styled exposing (Html, div, span, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model exposing (Notification(..))
import Msg exposing (Msg(..))
import View.Common
    exposing
        ( highlightColor1
        , normalTextSize
        , phantomDiv
        , red
        , secondary
        , spinner
        , white
        , whiteStr
        )


viewNotif : Size -> Maybe Notification -> Html Msg
viewNotif winSize notif =
    let
        connLostText =
            "Connection lost, click here to reconnect."
    in
    case notif of
        Nothing ->
            phantomDiv

        Just SplashNotif ->
            splash winSize

        Just ReconnectingNotif ->
            notifView "Reconnecting..." highlightColor1 True False

        Just ConnectionLostNotif ->
            notifView connLostText red False True


notifView : String -> Color -> Bool -> Bool -> Html Msg
notifView msg bgColor loadingSpinner refresh =
    let
        baseStyle =
            [ displayFlex
            , alignItems center
            , property "gap" "10px"
            , backgroundColor bgColor
            , borderBottomLeftRadius (px 8)
            , borderBottomRightRadius (px 8)
            , padding4 (px 14) (px 60) (px 18) (px 60)
            , color white
            , fontWeight bold
            ]

        style_ =
            if refresh then
                cursor pointer :: baseStyle

            else
                baseStyle

        attrs =
            if refresh then
                [ css style_, onClick ReInitApp ]

            else
                [ css style_ ]
    in
    div
        [ css
            [ position absolute
            , top (px 0)
            , width (pct 100)
            , zIndex (int notifzIndex)
            , displayFlex
            , justifyContent center
            ]
        ]
        [ div attrs <|
            if loadingSpinner then
                [ spinner normalTextSize whiteStr
                , span [] [ text msg ]
                ]

            else
                [ span [] [ text msg ] ]
        ]


splash : Size -> Html msg
splash winSize =
    let
        ( w, h ) =
            winSize |> sizeToFloatSize

        style_ =
            [ position absolute
            , top (px 0)
            , width (px w)
            , height (px h)
            , backgroundColor <| rgba 224 224 224 0.6
            , zIndex (int notifzIndex)
            , displayFlex
            , alignItems center
            , justifyContent center
            ]

        theSpace =
            let
                keyFrames =
                    keyframes
                        [ ( 0, [ Animation.property "opacity" "1" ] )
                        , ( 60, [ Animation.property "opacity" "0.15" ] )
                        , ( 100, [ Animation.property "opacity" "1" ] )
                        ]

                duration =
                    8

                loadingStyle =
                    [ animationName keyFrames
                    , animationDuration (sec duration)
                    , property "animation-iteration-count" "infinite"
                    , textAlign center
                    ]

                text_ =
                    div
                        [ css [ marginTop (px 10) ] ]
                        [ span [] [ text "Opening " ]
                        , span [ css [ fontWeight bold ] ] [ text "TheSpace" ]
                        ]
            in
            div [ css [ displayFlex, flexDirection column, color secondary ] ]
                [ div [ css loadingStyle ] [ Icons.theSpace, text_ ] ]
    in
    div [ css style_ ] [ theSpace ]
