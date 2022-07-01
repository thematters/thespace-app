module View.Sidebar exposing (viewSidebar)

import Array
import Config
    exposing
        ( cellModalWidth
        , getAssetsLimit
        , lightColor
        , price
        , sidebarWidth
        , sidebarzIndex
        , tokenSign
        , zeroPrice
        )
import Contract.ERC20 exposing (allowance)
import Contract.Space exposing (Pixel)
import Css exposing (..)
import Data
    exposing
        ( Activity(..)
        , ColorId
        , Index
        , Price
        , RpcErrorKind(..)
        , Size
        , SortOrder(..)
        , WalletInfo(..)
        , fromWei
        , notOnChain
        , tokenDeposited
        , unsafeColorIdToHexColor
        , walletConnected
        , walletIsAddress
        )
import Data.Icon as Icon
import Data.Image exposing (logoDataUri)
import Dict
import Env exposing (env)
import Eth.Defaults exposing (zeroAddress)
import Eth.Units exposing (EthUnit(..))
import Html
import Html.Styled exposing (Html, button, div, img, input, text, toUnstyled)
import Html.Styled.Attributes as Attributes
    exposing
        ( css
        , fromUnstyled
        , href
        , src
        , title
        , type_
        , value
        )
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Lazy as Lazy
import InfiniteList as Inf
import Model
    exposing
        ( AppMode(..)
        , SidebarInfLists
        , SidebarInfoType(..)
        , SidebarMode
        , SidebarUIMode(..)
        , TaxInfo
        )
import Model.Assets as A
import Model.Playback as PB
import Msg exposing (Msg(..))
import View.Common exposing (..)



-- Sizes


edge : Float
edge =
    24


navHeight : Float
navHeight =
    36


acctInfoHeight : Float
acctInfoHeight =
    94


switchHeight : Float
switchHeight =
    20


heightWithoutLogs : Float
heightWithoutLogs =
    navHeight + acctInfoHeight + switchHeight + 20


entryHeight : Float
entryHeight =
    64


assetOpHeight : Float
assetOpHeight =
    60


progressBarWidth : Float
progressBarWidth =
    140



-- UI Helpers


coordinates : Bool -> Index -> Html Msg
coordinates prefix idx =
    div
        [ css [ cursor pointer, color secondary ]
        , onClick <| SelectCell ( idx, True )
        ]
        [ View.Common.coords secDiv prefix idx ]


box : ColorId -> Html msg
box colorId =
    let
        boxSize =
            25

        hexc =
            unsafeColorIdToHexColor colorId

        style_ =
            if lightColor hexc then
                [ displayFlex
                , backgroundColor (hex hexc)
                , width (px <| boxSize - 2)
                , height (px <| boxSize - 2)
                , border3 (px 1) solid bggray
                ]

            else
                [ displayFlex
                , backgroundColor (hex hexc)
                , width (px boxSize)
                , height (px boxSize)
                ]
    in
    div [ css style_ ] []


placeholder : List (Html msg) -> Html msg
placeholder e =
    let
        style_ =
            [ displayFlex
            , justifyContent center
            , marginTop (px 40)
            , width (pct 100)
            , color gray
            ]
    in
    div [ css style_ ] e



-- Infinite List


scrollAreaStyle : Float -> Int -> List Style
scrollAreaStyle areaH itemLength =
    let
        showScrollBar =
            (itemLength |> toFloat) * entryHeight > areaH

        leftMargin =
            if showScrollBar then
                edge - scrollBarWidth

            else
                edge
    in
    [ height (px areaH)
    , overflowX hidden
    , overflowY auto
    , property "-webkit-overflow-scrolling" "touch"
    , paddingLeft (px edge)
    , paddingRight (px leftMargin)
    ]


scrollConfig : Float -> (Int -> Int -> item -> Html.Html msg) -> Inf.Config item msg
scrollConfig areaH itemView =
    Inf.config
        { itemView = itemView
        , itemHeight = entryHeight |> floor |> Inf.withConstantHeight
        , containerHeight = floor areaH
        }


scrollArea : Float -> List item -> Inf.Model -> (Int -> Int -> item -> Html.Html msg) -> (Inf.Model -> msg) -> Html msg
scrollArea scrollH items infList itemView msg =
    div
        [ css <| scrollAreaStyle scrollH <| List.length items
        , Inf.onScroll msg |> fromUnstyled
        ]
        [ Inf.view (scrollConfig scrollH itemView) infList items
            |> Html.Styled.fromUnstyled
        ]


viewSidebar : ( AppMode, SidebarMode, Size ) -> PB.Playback -> WalletInfo -> List Activity -> A.Assets -> SidebarInfLists -> TaxInfo -> Html Msg
viewSidebar ( appMode, ( uiMode, infoType ), winSize ) playback wallet acts assets { actsInfList, assetsInfList } { mintTax } =
    let
        modalW =
            sidebarWidth

        modalH =
            let
                ( _, winH ) =
                    winSize
            in
            toFloat winH - 2 * modalEdge

        baseStyle =
            [ position absolute
            , zIndex (int sidebarzIndex)
            , top (px modalEdge)
            , left (px modalEdge)
            , width (px modalW)
            , modalBoxShadow
            , modalRadius
            , backgroundColor modalBackgroundColor
            ]

        collapsedStyle =
            height (px <| edge * 2 + navHeight) :: baseStyle

        expandedStyle =
            height (px modalH) :: baseStyle

        collapsedSidebar pb =
            div
                [ css collapsedStyle ]
                [ div [ css [ margin (px edge), marginBottom (px 0) ] ]
                    [ nav uiMode pb ]
                ]
    in
    case ( appMode, uiMode ) of
        ( Loading, _ ) ->
            collapsedSidebar playback

        ( Realtime, CollapsedSidebar ) ->
            collapsedSidebar playback

        ( Realtime, ExpandedSidebar ) ->
            div
                [ css expandedStyle ]
                [ div
                    [ css
                        [ margin (px edge)
                        , marginBottom (px 0)
                        , height (px heightWithoutLogs)
                        ]
                    ]
                    [ nav uiMode playback
                    , acctInfo wallet
                    , switch wallet infoType assets
                    ]
                , let
                    acts_ =
                        -- In theory we don't need lazy here,
                        -- but Elm seems couldn't figure it out sometimes.
                        Lazy.lazy5 viewActs
                            modalH
                            wallet
                            mintTax
                            actsInfList
                            acts

                    assets_ =
                        Lazy.lazy4 viewAssets
                            modalH
                            wallet
                            assetsInfList
                            assets

                    hide =
                        [ css [ display none ] ]
                  in
                  div [] <|
                    case infoType of
                        -- Same trick as canvas, using fixed layout
                        -- to avoid scroll reset problem
                        ActLogs ->
                            [ div [] [ acts_ ], div hide [ assets_ ] ]

                        AssetsManager ->
                            [ div hide [ acts_ ], div [] [ assets_ ] ]
                ]

        ( Playback, _ ) ->
            viewPlayback playback


spinnerPlaceholder : Html msg
spinnerPlaceholder =
    div
        [ css
            [ width (px 28)
            , height (px 28)
            , displayFlex
            , alignItems center
            ]
        ]
        [ spinner bigTextSize grayStr ]


nav : SidebarUIMode -> PB.Playback -> Html Msg
nav uiMode playback =
    let
        style_ =
            [ displayFlex
            , justifyContent spaceBetween
            , alignItems start
            , height (px navHeight)
            ]

        iconsStyle =
            [ displayFlex
            , alignItems center
            , property "gap" "20px"
            , marginTop (px -4)
            ]

        linkIcon url title_ icon =
            div [ css [ cursor pointer ], title title_ ]
                [ Html.Styled.a
                    [ css [ outline none ]
                    , Attributes.target "_blank"
                    , href url
                    ]
                    [ iconNormal icon ]
                ]

        info_ =
            linkIcon Config.aboutLink "About" Icon.theSpaceSmall

        discord =
            linkIcon Config.discordLink "Discord" Icon.discord

        playback_ =
            let
                title_ =
                    "Playback Recent History"
            in
            if PB.readyToEnter playback then
                div
                    [ css [ cursor pointer ]
                    , title title_
                    , onClick <| AppModeChange Playback
                    ]
                    [ iconNormal Icon.history ]

            else
                spinnerPlaceholder

        modeSwitch =
            div
                [ css [ cursor pointer ]
                , title <|
                    case uiMode of
                        CollapsedSidebar ->
                            "Activity"

                        ExpandedSidebar ->
                            "Collapse"
                , onClick <|
                    SidebarModeSwitch <|
                        case uiMode of
                            CollapsedSidebar ->
                                ExpandedSidebar

                            ExpandedSidebar ->
                                CollapsedSidebar
                ]
                [ iconNormal <|
                    case uiMode of
                        CollapsedSidebar ->
                            Icon.activity

                        ExpandedSidebar ->
                            Icon.chevronUp
                ]

        icons =
            div [ css iconsStyle ]
                [ info_
                , discord
                , playback_
                , modeSwitch
                ]
    in
    div
        [ css style_ ]
        [ logo
        , div [ css [ color secondary ] ] [ icons ]
        ]


logo : Html msg
logo =
    img [ src logoDataUri, css [ height (px navHeight) ] ] []


acctInfo : WalletInfo -> Html Msg
acctInfo wallet =
    let
        style_ =
            [ displayFlex
            , flexDirection column
            , alignItems center
            , justifyContent spaceAround
            , height (px <| acctInfoHeight)
            ]

        info addr blc =
            let
                containerStyle =
                    [ displayFlex
                    , flexDirection column
                    , justifyContent spaceBetween
                    , height (pct 48)
                    , width (pct 100)
                    ]

                ln label value =
                    div [ css [ displayFlex, justifyContent spaceBetween ] ]
                        [ label, value ]
            in
            div
                [ css containerStyle ]
                [ ln (secDiv "My Wallet") (greenAddressTag addr)
                , ln (secDiv "Balance") (priceTagNormal blc)
                ]
    in
    div
        [ css style_ ]
        [ case wallet of
            DetectingWallet ->
                loadingMetaMaskBtn

            NoWallet ->
                installMetaMaskBtn

            LockedWallet ->
                lockedMataMaskBtn

            Wallet { chainId, address, balance, allowance } ->
                if notOnChain chainId then
                    switchNetworkBtn

                else
                    case ( address, balance, allowance ) of
                        ( Nothing, _, _ ) ->
                            connectMetaMaskBtn

                        ( _, Nothing, _ ) ->
                            loadingMetaMaskBtn

                        ( _, _, Nothing ) ->
                            loadingMetaMaskBtn

                        ( Just addr, Just blc, Just alw ) ->
                            if blc == zeroPrice then
                                getTokenBtn

                            else if tokenDeposited alw then
                                info addr blc

                            else
                                depositBtn
        ]


switch : WalletInfo -> SidebarInfoType -> A.Assets -> Html Msg
switch wallet activeInfoType assets =
    let
        style_ =
            [ displayFlex
            , justifyContent spaceBetween
            , height (px switchHeight)
            , marginBottom (px 20)
            ]

        toggleStyle disabled changed =
            [ displayFlex
            , justifyContent center
            , width (pct 50)
            , pseudoClass "first-child" [ borderRight3 (px 2) solid secondary ]
            ]
                ++ (if disabled then
                        [ color lightgray ]

                    else if changed then
                        [ color green, cursor pointer ]

                    else
                        [ cursor pointer ]
                   )

        toggle t disabled changed name =
            let
                tglBaseStyle =
                    toggleStyle disabled changed

                tglStyle =
                    if t == activeInfoType then
                        fontWeight bold :: tglBaseStyle

                    else
                        tglBaseStyle

                attrs =
                    if disabled then
                        [ css tglStyle ]

                    else
                        [ css tglStyle, onClick <| SidebarInfoSwitch t ]
            in
            div attrs [ text name ]

        yourPixelsNotAvailable =
            not <| walletConnected wallet

        yourPixelsChanged =
            case assets of
                A.Loaded ast ->
                    A.changed ast

                _ ->
                    False
    in
    div
        [ css style_ ]
        [ toggle ActLogs False False "Activity"
        , toggle AssetsManager yourPixelsNotAvailable yourPixelsChanged "My Pixels"
        ]


viewActs : Float -> WalletInfo -> Maybe Price -> Inf.Model -> List Activity -> Html Msg
viewActs modalH wallet mintTax actsInfList acts =
    if List.length acts == 0 then
        placeholder <| [ textDiv "No new activity recieved yet." ]

    else
        let
            scrollH =
                modalH - heightWithoutLogs - edge

            itemView =
                \_ _ act -> viewActEntry wallet mintTax act |> toUnstyled
        in
        scrollArea scrollH acts actsInfList itemView ScrollActivity


viewActEntry : WalletInfo -> Maybe Price -> Activity -> Html Msg
viewActEntry wallet mintTax log =
    let
        style_ =
            [ displayFlex
            , flexDirection column
            , justifyContent center
            , height (px entryHeight)
            , borderBottom3 (px 1) solid lightbggray
            ]

        cssLog removed =
            let
                scratched =
                    if removed then
                        [ textDecoration lineThrough ]

                    else
                        []
            in
            css <| style_ ++ scratched

        lineMarginTop =
            4

        lineStyle =
            [ displayFlex
            , alignItems center
            , property "gap" "6px"
            , marginTop (px lineMarginTop)
            ]

        addr a =
            if walletIsAddress wallet a then
                greenAddressTag a

            else
                addressTag a

        coords =
            coordinates False

        ln es =
            div [ css lineStyle ] es

        transferLog idx from_ to_ p =
            [ if walletIsAddress wallet from_ then
                ln [ coords idx, grayDiv "bought from", addr from_ ]

              else
                ln [ addr to_, grayDiv "bought", coords idx ]
            , ln [ priceTag p ]
            ]

        colorLog idx owr c =
            [ div [ css [ displayFlex, justifyContent spaceBetween ] ]
                [ div [] [ ln [ addr owr, grayDiv "colored", coords idx ] ]
                , div [ css [ displayFlex, alignItems center ] ] [ box c ]
                ]
            ]

        priceResetLog idx p =
            [ ln [ grayDiv "TheSpace priced", coords idx ], ln [ priceTag p ] ]

        priceLog idx owr p =
            [ ln [ addr owr, grayDiv "priced", coords idx ]
            , ln [ priceTag p ]
            ]

        taxLog idx pyer amt =
            [ ln [ coords idx, grayDiv "taxed", addr pyer ]
            , ln [ priceTag amt ]
            ]

        incomeLog idx coller amt =
            [ ln [ addr coller, grayDiv "collected", coords idx ]
            , ln [ priceTag amt ]
            ]

        defaultLog idx from_ =
            let
                defaultLn =
                    ln [ coords idx, grayDiv "defaulted from", addr from_ ]
            in
            case mintTax of
                Just p ->
                    [ defaultLn, ln [ grayDiv "to", priceTag p ] ]

                Nothing ->
                    [ defaultLn ]

        requestLog idx =
            [ ln [ grayDiv "Request sent for", coords idx ] ]

        errorLog { code, message } =
            if env.debug then
                let
                    msg =
                        message |> String.trim |> String.left 40
                in
                [ ln
                    [ boldTextDiv <| redDiv <| "Error "
                    , secDiv <| "code " ++ String.fromInt code
                    ]
                , div
                    [ css <| lineStyle, title message ]
                    [ secDiv <| msg ++ "…" ]
                ]

            else
                [ ln [ redDiv "Huh...something seems wrong here." ] ]
    in
    case log of
        TransferAct { index, from, to, amount, removed } ->
            div
                [ cssLog removed ]
            <|
                transferLog index from to amount

        ColorAct { index, color, owner, removed } ->
            div
                [ cssLog removed ]
            <|
                colorLog index owner color

        PriceAct { index, price, owner, removed } ->
            div
                [ cssLog removed ]
            <|
                if owner == zeroAddress then
                    priceResetLog index price

                else
                    priceLog index owner price

        TaxAct { index, payer, amount, removed } ->
            div [ cssLog removed ] <| taxLog index payer amount

        UbiAct { index, collector, amount, removed } ->
            div [ cssLog removed ] <| incomeLog index collector amount

        DefaultAct { from, index, removed } ->
            div [ cssLog removed ] <| defaultLog index from

        TxSendAct index ->
            div [ cssLog False ] <| requestLog index

        ActError err ->
            div [ cssLog False ] <| errorLog err


viewAssets : Float -> WalletInfo -> Inf.Model -> A.Assets -> Html Msg
viewAssets modalH wallet actInfList assets =
    if walletConnected wallet then
        case assets of
            A.NotLoaded ->
                placeholder <|
                    [ div [ css [ displayFlex, flexDirection column ] ]
                        [ div
                            [ css [ marginTop (px 10), marginBottom (px 10) ] ]
                            [ text "Your pixels are not loaded." ]
                        , button
                            [ css buttonStyle, onClick LoadAssets ]
                            [ text "Load Now" ]
                        ]
                    ]

            A.Loading { loadedPages } ->
                let
                    per =
                        case loadedPages of
                            Nothing ->
                                ""

                            Just loadedPages_ ->
                                let
                                    hasLoaded x =
                                        case x of
                                            A.Page _ ->
                                                True

                                            _ ->
                                                False

                                    needLoadPages =
                                        loadedPages_ |> Array.length

                                    loadPages =
                                        loadedPages_
                                            |> Array.filter hasLoaded
                                            |> Array.length

                                    percent =
                                        loadPages * 100 // needLoadPages
                                in
                                String.fromInt percent ++ "%"
                in
                placeholder <|
                    [ div
                        [ css
                            [ displayFlex
                            , flexDirection column
                            , textAlign center
                            ]
                        ]
                        [ bouncingBalls bigTextSize grayStr
                        , div
                            [ css [ marginTop (px 10) ] ]
                            [ textDiv "Loading your pixels, hang on..."
                            , textDiv per
                            ]
                        ]
                    ]

            A.Loaded ast ->
                if List.length ast.list == 0 then
                    placeholder <| [ textDiv "You don't have any pixels yet." ]

                else
                    div []
                        [ viewAssetOps ast
                        , viewAssetsResult modalH ast actInfList
                        ]

    else
        placeholder [ textDiv "Please connect wallet first." ]


viewAssetOps : A.LoadedData -> Html Msg
viewAssetOps ({ total, exceededLimit, rank } as assets) =
    let
        bottomMargin =
            10

        refresh_ icon =
            div
                [ css [ displayFlex, cursor pointer, property "gap" "4px" ]
                , onClick RefreshAssets
                ]
                [ icon Icon.refreshSmall ]

        nomalRefresh =
            refresh_ iconLight

        changedRefresh =
            refresh_ iconGreen

        refresh asts =
            if A.changed asts then
                changedRefresh

            else
                nomalRefresh

        totalCount ttl =
            let
                totalNum =
                    if ttl == 0 || ttl == 1 then
                        secDiv <| "Total " ++ String.fromInt ttl ++ " Pixel"

                    else
                        secDiv <| "Total " ++ String.fromInt ttl ++ " Pixels"
            in
            if exceededLimit then
                let
                    showingStr =
                        "(showing " ++ String.fromInt getAssetsLimit ++ ")"

                    showing =
                        smallTextDiv <| grayDiv <| showingStr
                in
                div
                    [ css [ displayFlex, alignItems end, property "gap" "4px" ] ]
                    [ totalNum, showing ]

            else
                totalNum

        totalMonetaryInfo lbl ttl =
            div
                [ css [ displayFlex, alignItems end, property "gap" "4px" ] ]
                [ secDiv lbl, priceTagNormal ttl ]

        totalInfo =
            case rank of
                ( A.RankTime, _ ) ->
                    totalCount total

                ( A.RankPrice, _ ) ->
                    totalMonetaryInfo "Total" <| A.totalPrice assets

                ( A.RankTax, _ ) ->
                    totalMonetaryInfo "Tax ≈" <| A.totalTax assets

                ( A.RankUbi, _ ) ->
                    totalMonetaryInfo "Inc. ≈" <| A.totalUbi assets

        info =
            div
                [ css
                    [ displayFlex
                    , alignItems center
                    , justifyContent spaceBetween
                    ]
                ]
                [ totalInfo, refresh assets ]
    in
    div
        [ css
            [ displayFlex
            , flexDirection column
            , justifyContent spaceBetween
            , height (px <| assetOpHeight - bottomMargin)
            , margin4 (px 0) (px edge) (px bottomMargin) (px edge)
            ]
        ]
        [ viewRanks rank, info ]


viewRanks : A.Rank -> Html Msg
viewRanks ( sType, sOrder ) =
    let
        item sortType label =
            div
                [ css [ displayFlex, alignItems end, cursor pointer ]
                , onClick <|
                    SortAssets
                        ( sortType
                        , if sortType == sType && sOrder == Descend then
                            Ascend

                          else
                            Descend
                        )
                ]
                [ div
                    [ css [ marginRight (px 2) ] ]
                    [ if sortType == sType then
                        secDiv label

                      else
                        grayDiv label
                    ]
                , case sOrder of
                    Descend ->
                        indicator ( sortType, Descend ) "▼"

                    Ascend ->
                        indicator ( sortType, Ascend ) "▲"
                ]

        indicator indicatorSortInfo label =
            div
                [ css [ fontSize smallText ] ]
                [ if indicatorSortInfo == ( sType, sOrder ) then
                    secDiv label

                  else
                    phantomDiv
                ]
    in
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent spaceBetween
            , marginBottom (px 10)
            ]
        ]
        [ item A.RankTime "Recent"
        , item A.RankPrice "Price"
        , item A.RankTax "Tax"
        , item A.RankUbi "Inc."
        ]


viewAssetsResult : Float -> A.LoadedData -> Inf.Model -> Html Msg
viewAssetsResult modalH { rank, changes, fetchedPixels, collectedIds, list } assetsInfList =
    let
        scrollH =
            modalH - (heightWithoutLogs + edge + assetOpHeight)

        itemView =
            \_ _ p ->
                viewAssetResultEntry rank changes fetchedPixels collectedIds p
                    |> toUnstyled
    in
    scrollArea scrollH list assetsInfList itemView ScrollAssets


viewAssetResultEntry : A.Rank -> A.Changes -> A.FetchedPixels -> A.CollectedIds -> Pixel -> Html Msg
viewAssetResultEntry ( sortType, _ ) changes fetchedPixels collectedIds pixel =
    let
        ( pxl, tradeFlag ) =
            case Dict.get pixel.index changes of
                Just A.Bought ->
                    let
                        fetchPxl =
                            case A.getFetchedPixel pixel.index fetchedPixels of
                                Just p ->
                                    p

                                Nothing ->
                                    pixel
                    in
                    ( fetchPxl, boldTextDiv <| smallTextDiv <| greenDiv "New" )

                Just A.Sold ->
                    ( pixel, boldTextDiv <| smallTextDiv <| orangeDiv "Sold" )

                Just A.Updated ->
                    ( pixel, smallTextDiv <| greenDiv "Updated" )

                Nothing ->
                    ( pixel, phantomDiv )

        ( extraTag, value ) =
            case sortType of
                A.RankTax ->
                    ( secDiv "Tax ≈", priceTag pxl.tax )

                A.RankUbi ->
                    ( if A.pixelCollected pxl.index collectedIds then
                        phantomDiv

                      else
                        secDiv "Income ≈"
                    , if A.pixelCollected pxl.index collectedIds then
                        div
                            [ css [ color green ]
                            , title <|
                                tokenSign
                                    ++ " "
                                    ++ fromWei Ether pxl.ubi
                            ]
                            [ text "√ Collected" ]

                      else
                        priceTag pxl.ubi
                    )

                _ ->
                    ( phantomDiv, priceTag pxl.price )
    in
    div
        [ css
            [ displayFlex
            , alignItems center
            , height (px entryHeight)
            , borderBottom3 (px 1) solid lightbggray
            ]
        ]
        [ div [ css [ displayFlex, marginRight (px 14) ] ] [ box pxl.color ]
        , div [ css [ displayFlex, flexDirection column, width (pct 100) ] ]
            [ div
                [ css [ displayFlex, justifyContent spaceBetween ] ]
                [ div
                    [ css [ displayFlex, property "gap" "4px" ] ]
                    [ coordinates True pxl.index, extraTag ]
                , tradeFlag
                ]
            , value
            ]
        ]


viewPlayback : PB.Playback -> Html Msg
viewPlayback pb =
    let
        modelStyle =
            [ position absolute
            , zIndex (int sidebarzIndex)
            , top (px modalEdge)
            , left (px modalEdge)
            , minWidth (px cellModalWidth)
            , modalBoxShadow
            , modalRadius
            , backgroundColor modalBackgroundColor
            , height (px <| navHeight + edge * 2)
            ]

        playbackStyle =
            [ displayFlex
            , justifyContent spaceBetween
            , alignItems center
            , height (pct 100)
            , marginLeft (px edge)
            , marginRight (px edge)
            ]
    in
    if PB.readyToEnter pb then
        div [ css modelStyle ]
            [ div
                [ css playbackStyle ]
                [ div [ css [ marginRight (px 10) ] ] [ logo ]
                , togglePlay pb
                , progress pb
                , circleSpeed pb
                , exitPlayback
                ]
            ]

    else
        phantomDiv


togglePlay : PB.Playback -> Html Msg
togglePlay pb =
    if not <| PB.readyToPlay pb then
        spinnerPlaceholder

    else if PB.playing pb then
        div
            [ css [ cursor pointer ]
            , title "Pause"
            , onClick PlaybackPause
            ]
            [ iconNormal Icon.pause ]

    else
        div
            [ css [ cursor pointer ]
            , title "Play"
            , onClick PlaybackPlay
            ]
            [ iconNormal Icon.play ]


progress : PB.Playback -> Html Msg
progress pb =
    if not <| PB.readyToPlay pb then
        div
            [ css
                [ width (px progressBarWidth)
                , borderBottom3 (px 2) solid lightgray
                ]
            ]
            []

    else
        let
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
                , backgroundColor secondary
                , property "-webkit-appearance" "none"
                , property "appearance" "none"
                , property "opacity" "0.7"
                , property "-webkit-transition" ".2s"
                , property "transition" "opacity .2s"
                , pseudoClass "-webkit-slider-thumb" <|
                    progressPseudoStyle
                        ++ [ property "appearance" "none"
                           , property "-webkit-appearance" "none"
                           ]
                , pseudoClass "-moz-range-thumb" progressPseudoStyle
                ]
        in
        input
            [ type_ "range"
            , Attributes.min <| String.fromInt 0
            , Attributes.max <| String.fromInt <| PB.maxProgress pb
            , value <| String.fromInt <| PB.currentProgress pb
            , onInput
                (\v ->
                    case String.toInt v of
                        Just i ->
                            PlaybackSlide i

                        Nothing ->
                            NoOp
                )
            , css progressStyle
            ]
            []


circleSpeed : PB.Playback -> Html Msg
circleSpeed pb =
    let
        baseStyle =
            [ fontSize extraBigText ]

        title_ =
            "Change Speed"
    in
    if not <| PB.readyToPlay pb then
        div [ css <| baseStyle ++ [ color lightgray ], title title_ ]
            [ text "1X" ]

    else
        div
            [ css <| baseStyle ++ [ color secondary, cursor pointer ]
            , title title_
            , onClick PlaybackCircleSpeed
            ]
            [ text <| PB.speedToString <| PB.currentSpeed pb ]


exitPlayback : Html Msg
exitPlayback =
    div
        [ css [ marginLeft (px 10), cursor pointer ]
        , title "Exit Playback"
        , onClick <| AppModeChange Realtime
        ]
        [ iconNormal Icon.close ]
