module Data.Icon exposing (..)

import Html.Styled exposing (Html)
import Svg.Styled as Svg exposing (Svg, svg)
import Svg.Styled.Attributes exposing (..)



-- Some SVG Icons from https://1602.github.io/elm-feather-icons/


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , height "24"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "24"
        ]


activity : Html msg
activity =
    svgFeatherIcon "activity"
        [ Svg.polyline [ points "22 12 18 12 15 21 9 3 6 12 2 12" ] []
        ]


arrowLeft : Html msg
arrowLeft =
    svgFeatherIcon "arrow-left"
        [ Svg.line [ x1 "19", y1 "12", x2 "5", y2 "12" ] []
        , Svg.polyline [ points "12 19 5 12 12 5" ] []
        ]


arrowRight : Html msg
arrowRight =
    svgFeatherIcon "arrow-right"
        [ Svg.line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        , Svg.polyline [ points "12 5 19 12 12 19" ] []
        ]


arrowUpRight : Html msg
arrowUpRight =
    svgFeatherIcon "arrow-up-right"
        [ Svg.line [ x1 "7", y1 "17", x2 "17", y2 "7" ] []
        , Svg.polyline [ points "7 7 17 7 17 17" ] []
        ]


chevronDown : Html msg
chevronDown =
    svgFeatherIcon "chevron-down"
        [ Svg.polyline [ points "6 9 12 15 18 9" ] []
        ]


chevronUp : Html msg
chevronUp =
    svgFeatherIcon "chevron-up"
        [ Svg.polyline [ points "18 15 12 9 6 15" ] []
        ]


clock : Html msg
clock =
    svgFeatherIcon "clock"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.polyline [ points "12 6 12 12 16 14" ] []
        ]


close : Html msg
close =
    svgFeatherIcon "x"
        [ Svg.line [ x1 "18", y1 "6", x2 "6", y2 "18" ] []
        , Svg.line [ x1 "6", y1 "6", x2 "18", y2 "18" ] []
        ]


crosshair : Html msg
crosshair =
    svgFeatherIcon "crosshair"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "22", y1 "12", x2 "18", y2 "12" ] []
        , Svg.line [ x1 "6", y1 "12", x2 "2", y2 "12" ] []
        , Svg.line [ x1 "12", y1 "6", x2 "12", y2 "2" ] []
        , Svg.line [ x1 "12", y1 "22", x2 "12", y2 "18" ] []
        ]


helpCircle : Html msg
helpCircle =
    svgFeatherIcon "help-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.path [ d "M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3" ] []
        , Svg.line [ x1 "12", y1 "17", x2 "12.01", y2 "17" ] []
        ]


info : Html msg
info =
    svgFeatherIcon "info"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "12", y1 "16", x2 "12", y2 "12" ] []
        , Svg.line [ x1 "12", y1 "8", x2 "12.01", y2 "8" ] []
        ]


maximize : Html msg
maximize =
    svgFeatherIcon "maximize"
        [ Svg.path [ d "M8 3H5a2 2 0 0 0-2 2v3m18 0V5a2 2 0 0 0-2-2h-3m0 18h3a2 2 0 0 0 2-2v-3M3 16v3a2 2 0 0 0 2 2h3" ] []
        ]


move : Html msg
move =
    svgFeatherIcon "move"
        [ Svg.polyline [ points "5 9 2 12 5 15" ] []
        , Svg.polyline [ points "9 5 12 2 15 5" ] []
        , Svg.polyline [ points "15 19 12 22 9 19" ] []
        , Svg.polyline [ points "19 9 22 12 19 15" ] []
        , Svg.line [ x1 "2", y1 "12", x2 "22", y2 "12" ] []
        , Svg.line [ x1 "12", y1 "2", x2 "12", y2 "22" ] []
        ]


pause : Html msg
pause =
    svgFeatherIcon "pause"
        [ Svg.rect [ x "6", y "4", width "4", height "16" ] []
        , Svg.rect [ x "14", y "4", width "4", height "16" ] []
        ]


play : Html msg
play =
    svgFeatherIcon "play"
        [ Svg.polygon [ points "5 3 19 12 5 21 5 3" ] []
        ]


refreshCw : Html msg
refreshCw =
    svgFeatherIcon "refresh-cw"
        [ Svg.polyline [ points "23 4 23 10 17 10" ] []
        , Svg.polyline [ points "1 20 1 14 7 14" ] []
        , Svg.path [ d "M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15" ] []
        ]


scissors : Html msg
scissors =
    svgFeatherIcon "scissors"
        [ Svg.circle [ cx "6", cy "6", r "3" ] []
        , Svg.circle [ cx "6", cy "18", r "3" ] []
        , Svg.line [ x1 "20", y1 "4", x2 "8.12", y2 "15.88" ] []
        , Svg.line [ x1 "14.47", y1 "14.48", x2 "20", y2 "20" ] []
        , Svg.line [ x1 "8.12", y1 "8.12", x2 "12", y2 "12" ] []
        ]


send : Html msg
send =
    svgFeatherIcon "send"
        [ Svg.line [ x1 "22", y1 "2", x2 "11", y2 "13" ] []
        , Svg.polygon [ points "22 2 15 22 11 13 2 9 22 2" ] []
        ]


share2 : Html msg
share2 =
    svgFeatherIcon "share-2"
        [ Svg.circle [ cx "18", cy "5", r "3" ] []
        , Svg.circle [ cx "6", cy "12", r "3" ] []
        , Svg.circle [ cx "18", cy "19", r "3" ] []
        , Svg.line [ x1 "8.59", y1 "13.51", x2 "15.42", y2 "17.49" ] []
        , Svg.line [ x1 "15.41", y1 "6.51", x2 "8.59", y2 "10.49" ] []
        ]


shoppingCart : Html msg
shoppingCart =
    svgFeatherIcon "shopping-cart"
        [ Svg.circle [ cx "9", cy "21", r "1" ] []
        , Svg.circle [ cx "20", cy "21", r "1" ] []
        , Svg.path [ d "M1 1h4l2.68 13.39a2 2 0 0 0 2 1.61h9.72a2 2 0 0 0 2-1.61L23 6H6" ] []
        ]


skipBack : Html msg
skipBack =
    svgFeatherIcon "skip-back"
        [ Svg.polygon [ points "19 20 9 12 19 4 19 20" ] []
        , Svg.line [ x1 "5", y1 "19", x2 "5", y2 "5" ] []
        ]


skipForward : Html msg
skipForward =
    svgFeatherIcon "skip-forward"
        [ Svg.polygon [ points "5 4 15 12 5 20 5 4" ] []
        , Svg.line [ x1 "19", y1 "5", x2 "19", y2 "19" ] []
        ]


slash : Html msg
slash =
    svgFeatherIcon "slash"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "4.93", y1 "4.93", x2 "19.07", y2 "19.07" ] []
        ]


zoomIn : Html msg
zoomIn =
    svgFeatherIcon "zoom-in"
        [ Svg.circle [ cx "11", cy "11", r "8" ] []
        , Svg.line [ x1 "21", y1 "21", x2 "16.65", y2 "16.65" ] []
        , Svg.line [ x1 "11", y1 "8", x2 "11", y2 "14" ] []
        , Svg.line [ x1 "8", y1 "11", x2 "14", y2 "11" ] []
        ]


zoomOut : Html msg
zoomOut =
    svgFeatherIcon "zoom-out"
        [ Svg.circle [ cx "11", cy "11", r "8" ] []
        , Svg.line [ x1 "21", y1 "21", x2 "16.65", y2 "16.65" ] []
        , Svg.line [ x1 "8", y1 "11", x2 "14", y2 "11" ] []
        ]



-- Custom Ones (an unified API should be better)


heartSmall : Html msg
heartSmall =
    svg
        [ class <| "feather feather-heart-sm"
        , fill "none"
        , height "16"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "16"
        ]
        [ Svg.path [ d "M20.84 4.61a5.5 5.5 0 0 0-7.78 0L12 5.67l-1.06-1.06a5.5 5.5 0 0 0-7.78 7.78l1.06 1.06L12 21.23l7.78-7.78 1.06-1.06a5.5 5.5 0 0 0 0-7.78z" ] []
        ]


maximizeBig : Html msg
maximizeBig =
    svg
        [ class <| "feather feather-maximize-x"
        , fill "none"
        , height "36"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "36"
        ]
        [ Svg.path [ d "M8 3H5a2 2 0 0 0-2 2v3m18 0V5a2 2 0 0 0-2-2h-3m0 18h3a2 2 0 0 0 2-2v-3M3 16v3a2 2 0 0 0 2 2h3" ] []
        ]


polygon : Html msg
polygon =
    svg
        [ class <| "polygon polygon-sm"
        , fill "rgb(208, 208, 208)"
        , color "rgb(192, 192, 192)"
        , height "18"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "0"
        , viewBox "0 0 48 48"
        , width "18"
        ]
        [ Svg.path [ d "M16.942 10c2.404 1.362 4.792 2.751 7.22 4.076 1.141.624 1.725 1.468 1.639 2.735-.048.478-.045.959.007 1.436.123.852-.276 1.323-1.048 1.67-.72.326-1.393.746-2.08 1.137-.386.22-.537.123-.53-.297.008-.718 0-1.435 0-2.153.01-.993-.404-1.744-1.313-2.252a176.409 176.409 0 0 1-3.531-2.028c-.89-.52-1.77-.574-2.684-.054-1.227.689-2.46 1.367-3.7 2.035-.91.49-1.248 1.274-1.266 2.21-.024 1.315-.02 2.63 0 3.944.015 1.04.486 1.807 1.467 2.324 1.158.608 2.27 1.291 3.41 1.941.943.534 1.885.536 2.825-.027 4.343-2.593 8.88-4.864 13.254-7.4 1.014-.585 2.004-.574 3.035.013 2.122 1.209 4.245 2.42 6.417 3.544 1.36.703 1.992 1.639 1.932 3.157-.094 2.46-.04 4.924-.018 7.387.01 1.122-.48 1.894-1.498 2.44-2.517 1.365-5.014 2.767-7.52 4.162h-1.79c-2.52-1.41-5.03-2.834-7.562-4.222-.888-.488-1.348-1.197-1.396-2.143-.042-.788-.013-1.579-.015-2.367a.777.777 0 0 1 .114-.445.825.825 0 0 1 .349-.313c.892-.478 1.777-.968 2.657-1.47.418-.236.599-.125.599.32 0 .766.013 1.53 0 2.297-.018.385.078.768.277 1.103.199.335.492.61.846.793 1.246.706 2.497 1.402 3.753 2.088.395.235.852.358 1.317.356.465-.003.92-.13 1.313-.37 1.182-.667 2.36-1.34 3.544-2.01.853-.477 1.26-1.168 1.241-2.128a92.832 92.832 0 0 1 0-4.087c.024-.993-.41-1.692-1.29-2.177-1.08-.595-2.172-1.174-3.231-1.804-1.167-.697-2.247-.642-3.43.031-4.228 2.397-8.5 4.722-12.738 7.107-1.138.64-2.165.627-3.294-.027a211.759 211.759 0 0 0-6.406-3.564c-.843-.452-1.497-1.004-1.818-1.897v-9.187c.289-.78.87-1.283 1.613-1.69C10.132 12.816 12.63 11.4 15.136 10h1.806Z" ] []
        ]


refreshSmall : Html msg
refreshSmall =
    svg
        [ class <| "feather feather-sm"
        , fill "none"
        , height "18"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "18"
        ]
        [ Svg.polyline [ points "23 4 23 10 17 10" ] []
        , Svg.polyline [ points "1 20 1 14 7 14" ] []
        , Svg.path [ d "M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15" ] []
        ]


theSpace : Html msg
theSpace =
    svg
        [ class <| "thespace thespace-sm"
        , height "120"
        , width "120"
        , fill "currentColor"
        , stroke "none"
        , strokeWidth "0"
        , viewBox "0 0 41 41"
        ]
        [ Svg.path [ d "M18.8035 9.10583H8.75903V19.1503H18.8035V9.10583Z" ] []
        , Svg.path [ d "M0 0V40.7248H40.7248V0H0ZM36.6511 36.6511H4.07374V4.07374H36.6511V36.6511Z" ] []
        ]
