module View.Common exposing (..)

import Config exposing (rpcProvider, tokenSign)
import Css exposing (..)
import Css.Global
import Data
    exposing
        ( ColorHex
        , Index
        , Position
        , Price
        , addressAbbr
        , cellString
        , fromWei
        , indexToCell
        , priceAbbrLong
        , priceAbbrNormal
        , priceAbbrShort
        )
import Data.Font as Font
import Data.Icon as Icons
import Eth.Types exposing (Address)
import Eth.Units exposing (EthUnit(..))
import Eth.Utils exposing (addressToString)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Html.Events.Extra.Wheel as Wheel
import Html.Styled exposing (Html, button, div, fromUnstyled, text)
import Html.Styled.Attributes exposing (css, href, title)
import Html.Styled.Events exposing (onClick)
import Loading exposing (LoaderType(..), LoadingState(..), defaultConfig, render)
import Model exposing (AppMode(..))
import Msg exposing (Msg(..))
import Svg.Styled
import Svg.Styled.Attributes



-- Gloabl Size


smallTextSize : Float
smallTextSize =
    14


normalTextSize : Float
normalTextSize =
    16


bigTextSize : Float
bigTextSize =
    18


smallText : Px
smallText =
    px smallTextSize


normalText : Px
normalText =
    px normalTextSize


bigText : Px
bigText =
    px bigTextSize



-- Global Color String


whiteStr : String
whiteStr =
    "fff"


blackStr : String
blackStr =
    "333"


secondaryStr : String
secondaryStr =
    "666"


grayStr : String
grayStr =
    "999"


lightgrayStr : String
lightgrayStr =
    "ccc"


bggrayStr : String
bggrayStr =
    "ddd"


lightbggrayStr : String
lightbggrayStr =
    "eee"


greenStr : String
greenStr =
    "4caf50"


redStr : String
redStr =
    "dc262a"


orangeStr : String
orangeStr =
    "ffa048"


highlightColor1Str : String
highlightColor1Str =
    "33cbfb"


highlightColor2Str : String
highlightColor2Str =
    "a7f8fe"



-- Global Color Hex


white : Css.Color
white =
    hex whiteStr


black : Css.Color
black =
    hex blackStr


secondary : Css.Color
secondary =
    hex secondaryStr


gray : Css.Color
gray =
    hex grayStr


lightgray : Css.Color
lightgray =
    hex lightgrayStr


bggray : Css.Color
bggray =
    hex bggrayStr


lightbggray : Css.Color
lightbggray =
    hex lightbggrayStr


green : Css.Color
green =
    hex greenStr


red : Css.Color
red =
    hex redStr


orange : Css.Color
orange =
    hex orangeStr


highlightColor1 : Css.Color
highlightColor1 =
    hex highlightColor1Str


highlightColor2 : Css.Color
highlightColor2 =
    hex highlightColor2Str



-- Modal


modalEdge : Float
modalEdge =
    20


modalBorder : Style
modalBorder =
    border3 (px 1) solid bggray


modalRadius : Style
modalRadius =
    borderRadius (px modalEdge)


modalBoxShadow : Style
modalBoxShadow =
    boxShadow5 (px 0) (px 0) (px 2) (px 0) (Css.rgba 128 128 128 0.4)


modalBackgroundColor : Css.Color
modalBackgroundColor =
    Css.rgba 255 255 255 0.88


modalBackgroundColorInvert : Css.Color
modalBackgroundColorInvert =
    Css.rgba 48 48 48 0.88



-- Cell Highlighting


highlight : Float -> Float -> Float -> Float -> Float -> ColorHex -> Int -> Html msg
highlight x y w h d color zidx =
    {-
       Note: this function may cause runtime exception.
       For now, it's the caller's job to perform appropriate check,
       like NaN/Infinity and w > 2*d && d > 2*d.
    -}
    div
        [ css
            [ position absolute
            , left (px x)
            , top (px y)
            , pointerEvents none
            , zIndex (int zidx)
            ]
        ]
        [ svgFrame w h d (w - 2 * d) (h - 2 * d) ("#" ++ color) ]


svgFrame : Float -> Float -> Float -> Float -> Float -> ColorHex -> Html msg
svgFrame w h d dw dh color =
    let
        w_ =
            String.fromInt <| floor w

        h_ =
            String.fromInt <| floor h

        viewBox =
            "0 0 " ++ w_ ++ " " ++ h_
    in
    Svg.Styled.svg
        [ Svg.Styled.Attributes.width w_
        , Svg.Styled.Attributes.height h_
        , Svg.Styled.Attributes.viewBox viewBox
        , Svg.Styled.Attributes.fill color
        ]
        [ Svg.Styled.path
            [ Svg.Styled.Attributes.d <| framePath w h d dw dh ]
            []
        ]


framePath : Float -> Float -> Float -> Float -> Float -> String
framePath w_ h_ d_ dw_ dh_ =
    let
        w =
            String.fromInt <| floor w_

        h =
            String.fromInt <| floor h_

        d =
            String.fromInt <| floor d_

        dw =
            String.fromInt <| floor dw_

        dh =
            String.fromInt <| floor dh_
    in
    ("M0,0h" ++ w ++ "v" ++ h ++ "h-" ++ w ++ "z")
        ++ ("M" ++ d ++ "," ++ d ++ "v" ++ dh ++ "h" ++ dw ++ "v-" ++ dh ++ "z")


buttonHeight : Float
buttonHeight =
    46


buttonBasicStyle : List Style
buttonBasicStyle =
    [ displayFlex
    , justifyContent center
    , alignItems center
    , height (px buttonHeight)
    , width (pct 100)
    , borderRadius (px 20)
    , textDecoration none
    , focus [ outline none ]
    , fontWeight bold
    ]


buttonStyle : List Style
buttonStyle =
    [ color black
    , backgroundColor <| Css.rgba 255 255 255 0.3
    , border3 (px 2) solid black
    , cursor pointer
    ]
        ++ buttonBasicStyle


buttonInvertStyle : List Style
buttonInvertStyle =
    [ color white
    , backgroundColor <| Css.rgba 3 3 3 0.9
    , border3 (px 2) solid black
    , cursor pointer
    ]
        ++ buttonBasicStyle


buttonDisabledStyle : List Style
buttonDisabledStyle =
    [ color lightgray
    , backgroundColor <| Css.rgba 255 255 255 0.3
    , border3 (px 2) solid lightgray
    ]
        ++ buttonBasicStyle


icon_ : Color -> Html msg -> Html msg
icon_ c i =
    div [ css [ displayFlex, color c ] ] [ i ]


iconLight : Html msg -> Html msg
iconLight i =
    icon_ lightgray i


iconNormal : Html msg -> Html msg
iconNormal i =
    icon_ secondary i


iconInvert : Html msg -> Html msg
iconInvert i =
    icon_ gray i


iconBlack : Html msg -> Html msg
iconBlack i =
    icon_ black i


iconWhite : Html msg -> Html msg
iconWhite i =
    icon_ white i


iconGreen : Html msg -> Html msg
iconGreen i =
    icon_ green i



-- Loading Spinner


spinner : Float -> String -> Html msg
spinner size color =
    let
        config =
            { defaultConfig | color = "#" ++ color, size = size }
    in
    fromUnstyled <| render BouncingBalls config On



-- Misc


phantomDiv : Html msg
phantomDiv =
    div [ css [ display none ] ] []


placeholderDiv : Html msg
placeholderDiv =
    div [] []


boldTextDiv : Html msg -> Html msg
boldTextDiv e =
    div [ css [ fontWeight bold ] ] [ e ]


bigTextDiv : Html msg -> Html msg
bigTextDiv e =
    div [ css [ fontSize bigText ] ] [ e ]


smallTextDiv : Html msg -> Html msg
smallTextDiv e =
    div [ css [ fontSize smallText ] ] [ e ]


grayDiv : String -> Html msg
grayDiv s =
    div [ css [ color gray ] ] [ text s ]


lightgrayDiv : String -> Html msg
lightgrayDiv s =
    div [ css [ color lightgray ] ] [ text s ]


secDiv : String -> Html msg
secDiv s =
    div [ css [ color secondary ] ] [ text s ]


greenDiv : String -> Html msg
greenDiv s =
    div [ css [ color green ] ] [ text s ]


redDiv : String -> Html msg
redDiv s =
    div [ css [ color red ] ] [ text s ]


orangeDiv : String -> Html msg
orangeDiv s =
    div [ css [ color orange ] ] [ text s ]


textDiv : String -> Html msg
textDiv s =
    div [] [ text s ]


priceTag_ : (Price -> String) -> Price -> Html msg
priceTag_ abbr v =
    div
        [ css
            [ displayFlex
            , alignItems center
            , property "gap" "2px"
            , whiteSpace noWrap
            ]
        , title <| tokenSign ++ " " ++ fromWei Ether v
        ]
        [ grayDiv tokenSign, textDiv <| abbr v ]


priceTag : Price -> Html msg
priceTag =
    priceTag_ priceAbbrLong


priceTagNormal : Price -> Html msg
priceTagNormal =
    priceTag_ priceAbbrNormal


priceTagShort : Price -> Html msg
priceTagShort =
    priceTag_ priceAbbrShort


addressTag : Address -> Html msg
addressTag addr =
    div
        [ css [ whiteSpace noWrap ]
        , title <| addressToString addr
        ]
        [ textDiv <| addressAbbr addr ]


greenAddressTag : Address -> Html msg
greenAddressTag addr =
    div
        [ css [ whiteSpace noWrap, color green ]
        , title <| addressToString addr
        ]
        [ textDiv <| addressAbbr addr ]


coords : (String -> Html msg) -> Bool -> Index -> Html msg
coords colorDiv prefix idx =
    let
        s =
            idx |> indexToCell |> cellString
    in
    div [ css [ displayFlex, alignItems center ] ]
        [ colorDiv <|
            if prefix then
                "Pixel " ++ s

            else
                s
        ]


installButtonInfo : String
installButtonInfo =
    "To buy pixel, your need to install MetaMask wallet."


getTokenButtonInfo : String
getTokenButtonInfo =
    "Your don't have any " ++ tokenSign ++ ", here is how to get some."


switchButtonInfo : String
switchButtonInfo =
    "To buy pixel, you need to switch your wallet to Polygon. "


connectMetaMaskInfo : String
connectMetaMaskInfo =
    "To buy pixel, you need to connect your wallet first."


depositButtonInfo : String
depositButtonInfo =
    "To buy pixel, you need to approve your " ++ tokenSign ++ "."


syncPriceInfo : String
syncPriceInfo =
    "Price changed after you opened the window, click here to sync."


opLinkBtn : String -> String -> String -> Html Msg
opLinkBtn link title_ s =
    Html.Styled.a
        [ css buttonStyle
        , href link
        , title title_
        , Html.Styled.Attributes.target "_blank"
        ]
        [ text s ]


loadingMetaMaskBtn : Html msg
loadingMetaMaskBtn =
    button [ css buttonDisabledStyle ] [ text "Checking MetaMask" ]


installMetaMaskBtn : Html Msg
installMetaMaskBtn =
    opLinkBtn Config.installMetaMaskLink installButtonInfo "Install MetaMask"


lockedMataMaskBtn : Html Msg
lockedMataMaskBtn =
    button [ css buttonDisabledStyle ] [ text "MetaMask Locked" ]


switchNetworkBtn : Html Msg
switchNetworkBtn =
    button
        [ css buttonStyle
        , title switchButtonInfo
        , onClick <| SwitchWalletNetwork rpcProvider
        ]
        [ text "Switch to Polygon" ]


connectMetaMaskBtn : Html Msg
connectMetaMaskBtn =
    button
        [ css buttonStyle
        , title connectMetaMaskInfo
        , onClick ConnectMetaMask
        ]
        [ text "Connect MetaMask" ]


getTokenBtn : Html Msg
getTokenBtn =
    opLinkBtn Config.getTokenLink getTokenButtonInfo <| "Get " ++ tokenSign


depositBtn : Html Msg
depositBtn =
    button
        [ css buttonStyle
        , title depositButtonInfo
        , onClick <| RequestApproveAllBalance
        ]
        [ text <| "Approve Your " ++ tokenSign ]


opLinkStyle : List Style
opLinkStyle =
    [ displayFlex
    , alignItems center
    , height (px buttonHeight)
    , color black
    , fontWeight bold
    , cursor pointer
    ]


opLink : String -> String -> String -> Html Msg
opLink link title_ s =
    div
        [ css [ displayFlex, alignItems start, height (px buttonHeight) ] ]
        [ div [ css [ displayFlex, alignItems center ] ]
            [ Html.Styled.a
                [ css [ color black, fontWeight bold, textDecoration none ]
                , Html.Styled.Attributes.target "_blank"
                , href link
                , title title_
                ]
                [ text s ]
            ]
        , iconBlack Icons.arrowUpRight
        ]


installMetaMaskLink : Html Msg
installMetaMaskLink =
    opLink Config.installMetaMaskLink installButtonInfo "Install MetaMask"


switchNetworkLink : Html Msg
switchNetworkLink =
    div
        [ css opLinkStyle
        , title switchButtonInfo
        , onClick <| SwitchWalletNetwork rpcProvider
        ]
        [ text "Switch to Polygon" ]


connectMetaMaskLink : Html Msg
connectMetaMaskLink =
    div
        [ css opLinkStyle
        , title connectMetaMaskInfo
        , onClick ConnectMetaMask
        ]
        [ text "Connect MetaMask" ]


getTokenLink : Html Msg
getTokenLink =
    opLink Config.getTokenLink getTokenButtonInfo <| "Get " ++ tokenSign


depositLink : Html Msg
depositLink =
    div
        [ css opLinkStyle
        , title depositButtonInfo
        , onClick RequestApproveAllBalance
        ]
        [ text "Approve Now" ]


syncPriceLink : Html Msg
syncPriceLink =
    div
        [ css opLinkStyle
        , title syncPriceInfo
        , onClick SyncQuotePrice
        ]
        [ text "Sync Price" ]



-- Event Handlers


mouseDownHandler_ : (Position -> Msg) -> Mouse.Event -> Msg
mouseDownHandler_ msg evt =
    let
        ( x, y ) =
            evt.pagePos
    in
    msg { x = x, y = y }


mapMouseDownHandler : Mouse.Event -> Msg
mapMouseDownHandler =
    mouseDownHandler_ MapMouseDown


miniMapMouseDownHandler : Mouse.Event -> Msg
miniMapMouseDownHandler =
    mouseDownHandler_ MiniMapMouseDown


mouseWheelHandler : Wheel.Event -> Msg
mouseWheelHandler evt =
    if evt.deltaY > 0 then
        ZoomOut

    else
        ZoomIn


touchCoordinates : Touch.Event -> Position
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map (.clientPos >> (\( x, y ) -> { x = x, y = y }))
        |> Maybe.withDefault { x = 0, y = 0 }


mapTouchStartHandler : Touch.Event -> Msg
mapTouchStartHandler =
    touchCoordinates >> MapMouseDown


mapTouchEndHandler : Touch.Event -> Msg
mapTouchEndHandler =
    touchCoordinates >> MouseUp


mapTouchMoveHandler : Touch.Event -> Msg
mapTouchMoveHandler =
    touchCoordinates >> MouseMove


miniMapTouchStartHandler : Touch.Event -> Msg
miniMapTouchStartHandler =
    touchCoordinates >> MiniMapMouseDown


miniMapTouchEndHandler : Touch.Event -> Msg
miniMapTouchEndHandler =
    mapTouchEndHandler


miniMapTouchMoveHandler : Touch.Event -> Msg
miniMapTouchMoveHandler =
    mapTouchMoveHandler



-- Gloabl Style


scrollBarWidth : Float
scrollBarWidth =
    10


globalStyle : AppMode -> List (Html msg)
globalStyle appmode =
    -- set bgcolor / set font / disable user-select / scrollbar style
    let
        bgcolor =
            case appmode of
                RealtimeLoading ->
                    bggray

                Realtime ->
                    bggray

                PlaybackLoading ->
                    secondary

                Playback _ ->
                    secondary

        fontface =
            Css.Global.typeSelector "@font-face"
                [ property "font-family" "font"
                , property "src"
                    ("url('" ++ Font.lato ++ "') format('woff')")
                , Css.fontWeight Css.normal
                , Css.fontStyle Css.normal
                , property "font-display" "auto"
                ]

        fontStyle =
            [ fontSize normalText
            , property "font-family" "font"
            , property "user-select" "none"
            ]

        scrollbarStyle =
            [ Css.Global.typeSelector "::-webkit-scrollbar"
                [ width (px scrollBarWidth) ]
            , Css.Global.typeSelector "::-webkit-scrollbar-track"
                [ property "background" "transparent" ]
            , Css.Global.typeSelector "::-webkit-scrollbar-thumb"
                [ property "background" "transparent"
                , borderRadius (px 6)
                ]
            , Css.Global.typeSelector "::-webkit-scrollbar-thumb:hover"
                [ property "background" "#bbb" ]
            ]

        globalStyle_ =
            [ Css.Global.body [ backgroundColor bgcolor ]
            , fontface
            , Css.Global.body fontStyle
            , Css.Global.input fontStyle
            , Css.Global.button fontStyle
            ]
                ++ scrollbarStyle
    in
    [ Css.Global.global globalStyle_ ]
