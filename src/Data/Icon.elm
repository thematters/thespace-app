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
        [ class <| "polygon-sm"
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
        [ class <| "feather feather-refersh-sm"
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


spacePath : List (Html msg)
spacePath =
    [ Svg.path [ d "M18.8035 9.10583H8.75903V19.1503H18.8035V9.10583Z" ] []
    , Svg.path [ d "M0 0V40.7248H40.7248V0H0ZM36.6511 36.6511H4.07374V4.07374H36.6511V36.6511Z" ] []
    ]


theSpace : Html msg
theSpace =
    svg
        [ class <| "thespace"
        , height "120"
        , width "120"
        , fill "currentColor"
        , stroke "none"
        , strokeWidth "0"
        , viewBox "0 0 41 41"
        ]
        spacePath


theSpaceSmall : Html msg
theSpaceSmall =
    svg
        [ class <| "thespace-sm"
        , height "20"
        , width "20"
        , fill "currentColor"
        , stroke "none"
        , strokeWidth "0"
        , viewBox "0 0 41 41"
        ]
        spacePath


discord : Html msg
discord =
    svg
        [ class <| "discord"
        , height "26"
        , width "26"
        , fill "currentColor"
        , stroke "none"
        , strokeWidth "0"
        , viewBox "0 0 32 32"
        ]
        [ Svg.path [ d "M 12.65625 4.90625 L 11.875 5 C 11.875 5 8.371094 5.382813 5.8125 7.4375 L 5.78125 7.4375 L 5.75 7.46875 C 5.175781 7.996094 4.925781 8.644531 4.53125 9.59375 C 4.136719 10.542969 3.714844 11.753906 3.34375 13.09375 C 2.601563 15.777344 2 19.027344 2 22 L 2 22.25 L 2.125 22.5 C 3.050781 24.125 4.695313 25.160156 6.21875 25.875 C 7.742188 26.589844 9.058594 26.96875 9.96875 27 L 10.5625 27.03125 L 10.875 26.5 L 11.96875 24.5625 C 13.128906 24.824219 14.464844 25 16 25 C 17.535156 25 18.871094 24.824219 20.03125 24.5625 L 21.125 26.5 L 21.4375 27.03125 L 22.03125 27 C 22.941406 26.96875 24.257813 26.589844 25.78125 25.875 C 27.304688 25.160156 28.949219 24.125 29.875 22.5 L 30 22.25 L 30 22 C 30 19.027344 29.398438 15.777344 28.65625 13.09375 C 28.285156 11.753906 27.863281 10.542969 27.46875 9.59375 C 27.074219 8.644531 26.824219 7.996094 26.25 7.46875 L 26.21875 7.4375 L 26.1875 7.4375 C 23.628906 5.382813 20.125 5 20.125 5 L 19.34375 4.90625 L 19.0625 5.625 C 19.0625 5.625 18.773438 6.355469 18.59375 7.1875 C 17.460938 7.035156 16.535156 7 16 7 C 15.464844 7 14.539063 7.035156 13.40625 7.1875 C 13.226563 6.355469 12.9375 5.625 12.9375 5.625 Z M 11.28125 7.1875 C 11.324219 7.328125 11.367188 7.449219 11.40625 7.5625 C 10.113281 7.882813 8.734375 8.371094 7.46875 9.15625 L 8.53125 10.84375 C 11.125 9.234375 14.851563 9 16 9 C 17.148438 9 20.875 9.234375 23.46875 10.84375 L 24.53125 9.15625 C 23.265625 8.371094 21.886719 7.882813 20.59375 7.5625 C 20.632813 7.449219 20.675781 7.328125 20.71875 7.1875 C 21.652344 7.375 23.433594 7.804688 24.90625 8.96875 C 24.898438 8.972656 25.28125 9.550781 25.625 10.375 C 25.976563 11.222656 26.367188 12.351563 26.71875 13.625 C 27.394531 16.066406 27.925781 19.039063 27.96875 21.65625 C 27.339844 22.617188 26.171875 23.484375 24.9375 24.0625 C 23.859375 24.566406 23.007813 24.75 22.5 24.84375 L 22 24 C 22.296875 23.890625 22.589844 23.769531 22.84375 23.65625 C 24.382813 22.980469 25.21875 22.25 25.21875 22.25 L 23.90625 20.75 C 23.90625 20.75 23.34375 21.265625 22.03125 21.84375 C 20.71875 22.421875 18.714844 23 16 23 C 13.285156 23 11.28125 22.421875 9.96875 21.84375 C 8.65625 21.265625 8.09375 20.75 8.09375 20.75 L 6.78125 22.25 C 6.78125 22.25 7.617188 22.980469 9.15625 23.65625 C 9.410156 23.769531 9.703125 23.890625 10 24 L 9.5 24.84375 C 8.992188 24.75 8.140625 24.566406 7.0625 24.0625 C 5.828125 23.484375 4.660156 22.617188 4.03125 21.65625 C 4.074219 19.039063 4.605469 16.066406 5.28125 13.625 C 5.632813 12.351563 6.023438 11.222656 6.375 10.375 C 6.71875 9.550781 7.101563 8.972656 7.09375 8.96875 C 8.566406 7.804688 10.347656 7.375 11.28125 7.1875 Z M 12.5 14 C 11.726563 14 11.042969 14.441406 10.625 15 C 10.207031 15.558594 10 16.246094 10 17 C 10 17.753906 10.207031 18.441406 10.625 19 C 11.042969 19.558594 11.726563 20 12.5 20 C 13.273438 20 13.957031 19.558594 14.375 19 C 14.792969 18.441406 15 17.753906 15 17 C 15 16.246094 14.792969 15.558594 14.375 15 C 13.957031 14.441406 13.273438 14 12.5 14 Z M 19.5 14 C 18.726563 14 18.042969 14.441406 17.625 15 C 17.207031 15.558594 17 16.246094 17 17 C 17 17.753906 17.207031 18.441406 17.625 19 C 18.042969 19.558594 18.726563 20 19.5 20 C 20.273438 20 20.957031 19.558594 21.375 19 C 21.792969 18.441406 22 17.753906 22 17 C 22 16.246094 21.792969 15.558594 21.375 15 C 20.957031 14.441406 20.273438 14 19.5 14 Z M 12.5 16 C 12.554688 16 12.625 16.019531 12.75 16.1875 C 12.875 16.355469 13 16.648438 13 17 C 13 17.351563 12.875 17.644531 12.75 17.8125 C 12.625 17.980469 12.554688 18 12.5 18 C 12.445313 18 12.375 17.980469 12.25 17.8125 C 12.125 17.644531 12 17.351563 12 17 C 12 16.648438 12.125 16.355469 12.25 16.1875 C 12.375 16.019531 12.445313 16 12.5 16 Z M 19.5 16 C 19.554688 16 19.625 16.019531 19.75 16.1875 C 19.875 16.355469 20 16.648438 20 17 C 20 17.351563 19.875 17.644531 19.75 17.8125 C 19.625 17.980469 19.554688 18 19.5 18 C 19.445313 18 19.375 17.980469 19.25 17.8125 C 19.125 17.644531 19 17.351563 19 17 C 19 16.648438 19.125 16.355469 19.25 16.1875 C 19.375 16.019531 19.445313 16 19.5 16 Z" ] []
        ]
