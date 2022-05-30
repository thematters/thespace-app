module View.Sponsor exposing (viewSponsor)

import Config exposing (sidebarzIndex)
import Css exposing (..)
import Data.Icon exposing (heartSmall)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import View.Common exposing (iconLight, lightgray, smallText, textDiv)


viewSponsor : Html msg
viewSponsor =
    let
        heart =
            iconLight heartSmall

        poweredBy =
            textDiv "Powered by"

        mattersLab =
            div [ css [ fontWeight bold ] ] [ text "Matters Lab" ]
    in
    div
        [ css
            [ displayFlex
            , alignItems center
            , position absolute
            , top <| px 12
            , right <| px 18
            , zIndex <| int sidebarzIndex
            , color lightgray
            , fontSize smallText
            , backgroundColor <| Css.rgba 255 255 255 0
            , property "gap" "4px"
            , property "pointer-events" "none"
            ]
        ]
        [ heart, poweredBy, mattersLab ]
